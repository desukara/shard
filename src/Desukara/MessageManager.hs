{-# LANGUAGE OverloadedStrings, MultiWayIf #-}

module Desukara.MessageManager (
    messageManager
) where

import Control.Monad (when)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan
import Data.Bits
import Data.List (isInfixOf, intersect)
import Data.Time.Clock
import Data.Time.Format
import qualified Data.Text as T
import Text.Regex
import System.Random

import Discord
import qualified Discord as DS

import DbLib
import DbLib.GuildDataDb.Channels
import qualified DbLib.GuildDataDb.Channels as DB
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

                    -- when mentioned (handleMessage m)
                    handleMessage m
            _ -> return ()
        
        messageManager botid totalrunners chan dis ctx -- todo: repeatedly?
    where

        handleMessage msg = 
            do
                let text = T.unpack $ messageText msg
                    user = messageAuthor msg
                    username = userName user
                    userid = userId user
                    maybeGuild = messageGuild msg
                    ds_channel = messageChannel msg
                    channelId = show ds_channel

                    sendMessage t = do
                        restCall dis (CreateMessage ds_channel t Nothing) 
                        return ()

                -- todo: really inefficient...
                isChannelEnabled <- fmap ((channelId `elem`) . (map DB.channelId)) $ getActiveChannels ctx

                if | "ds!eval" `isInfixOf` text && isChannelEnabled -> 
                    do
                        let mkRegex' s = mkRegexWithOpts s False False 
                            rRegex = mkRegex' "```r(.*)```"

                        case matchRegex rRegex text of
                            Just matches -> do
                                let code = head matches 

                                winner <- randomRIO (0, totalrunners - 1) -- todo allocate smarter?
                                
                                currentTime <- getCurrentTime
                                createJob ctx defaultJob {
                                    jobTitle = username ++ "'s Query",
                                        -- ++ formatTime defaultTimeLocale "%D, %l:%M%P UTC" currentTime,
                                    jobOwner = winner,
                                    jobChannel = show (messageChannel msg),
                                    jobParameters = RScript {
                                        rsScript = code,
                                        rsTrusted = ""
                                    },
                                    jobRequestedChannelData = [ show (messageChannel msg) ] -- todo
                                } 
                            Nothing -> do
                                sendMessage "I couldn't find anything to run... (did you wrap your code with **```r** ?)"
                                return ()

                        return ()
                   | "ds@" `isInfixOf` text -> do
                        -- check perms
                        case maybeGuild of
                            Just guildId -> do
                                -- get guild owner
                                guildOwner <- do 
                                    maybeGuild <- restCall dis (GetGuild guildId)
                                    return $ case maybeGuild of
                                        Right guild -> show $ guildOwnerId guild
                                        Left _ -> ""

                                -- get admin roles
                                adminRoles <- do
                                   maybeRoles <- restCall dis (GetGuildRoles guildId)
                                   return $ case maybeRoles of
                                        Right roles -> map roleID $
                                            filter (((>) 0) . ((.&.) 8) . rolePerms) roles
                                        Left _ -> []
                                g <- restCall dis (GetGuildMember guildId (userId user)) 
                                
                                case g of
                                    Left _ -> return ()
                                    Right guildMember -> do
                                        let isAdmin = (show userid == guildOwner)
                                                   || (not . null $ memberRoles guildMember `intersect` adminRoles)
                                        if isAdmin
                                            -- verified!
                                            then if | "ds@enableChannel" `isInfixOf` text -> 
                                                        do 
                                                            setChannelEnabled ctx channelId True
                                                            sendMessage . T.pack $ "Enabled indexing and queries in this channel!"
                                                            return ()
                                                    | "ds@disableChannel" `isInfixOf` text ->
                                                        do
                                                            setChannelEnabled ctx channelId False 
                                                            sendMessage . T.pack $ "Disabled indexing and queries in this channel!"
                                                            return ()
                                                    | True -> return ()
                                            else sendMessage "Sorry, you don't have the permissions to do that..."
                                        return ()
                            Nothing -> return ()
                   | True -> return ()


            
               