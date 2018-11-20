{-# LANGUAGE OverloadedStrings, MultiWayIf #-}

module Desukara.MessageManager (
    messageManager
) where

import Control.Monad (when)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan
import Data.List (isInfixOf)
import Data.Time.Clock
import Data.Time.Format
import qualified Data.Text as T
import Text.Regex
import System.Random

import Discord
import qualified Discord as DS

import DbLib
import DbLib.JobManagerDb.Jobs

type TotalJobRunners = Int
type BotId = String

-- todo consistent dbcontext / others order in typesig
messageManager :: BotId
               -> TotalJobRunners
               -> Chan (Either GatewayException Event) -> (RestChan, Gateway, [ThreadIdType]) -> DbContext -> IO ()
messageManager botid totalrunners chan dis ctx = 
    do
        e <- readChan chan

        case e of
            Left _      -> return ()
            Right (MessageCreate m) ->
                do
                    let text = messageText m
                        channel = messageChannel m
                        mentions = messageMentions m
                        -- todo configurable
                        mentioned = [] /= filter (\u -> show (userId u) == botid) mentions

                    when mentioned (handleMessage m)
            _ -> return ()
        
        messageManager botid totalrunners chan dis ctx -- todo: repeatedly?
    where

        handleMessage msg = 
            do
                let text = T.unpack $ messageText msg
                    user = userName $ messageAuthor msg
                    ds_channel = messageChannel msg

                    sendMessage t =
                        restCall dis (CreateMessage ds_channel t Nothing) 

                if | "eval" `isInfixOf` text -> 
                    do
                        let mkRegex' s = mkRegexWithOpts s False False 
                            rRegex = mkRegex' "```r(.*)```"

                        case matchRegex rRegex text of
                            Just matches -> do
                                let code = head matches 

                                sendMessage "Sure thing!"

                                winner <- randomRIO (0, totalrunners - 1) -- todo allocate smarter?
                                
                                currentTime <- getCurrentTime
                                createJob ctx defaultJob {
                                    jobTitle = user ++ "'s Query",
                                        -- ++ formatTime defaultTimeLocale "%D, %l:%M%P UTC" currentTime,
                                    jobOwner = winner,
                                    jobChannel = show (messageChannel msg),
                                    jobParameters = RScript {
                                        rsScript = code,
                                        rsTrusted = ""
                                    }
                                } 
                            Nothing -> do
                                sendMessage "I couldn't find anything to run... (did you wrap your code with **```r** ?)"
                                return ()

                        return ()
                   | True -> return ()


            
               