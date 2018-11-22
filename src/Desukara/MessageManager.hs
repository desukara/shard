{-# LANGUAGE OverloadedStrings, MultiWayIf #-}

module Desukara.MessageManager (
    messageManager
) where

import Control.Monad (when)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan
import Data.Bits
import Data.Ord (compare)
import Data.List (sortBy, isInfixOf, intersect)
import Data.List.Split (splitOn, chunksOf)
import Data.Maybe (fromJust)
import Data.Time
import Data.Time.Clock
import Data.Time.Format
import qualified Data.Text as T
import Text.Regex
import System.Random

import Discord
import qualified Discord as DS

import Desukara.RCatalog

import DbLib
import DbLib.GuildDataDb.Channels
import qualified DbLib.GuildDataDb.Users as DB
import qualified DbLib.GuildDataDb.Messages as DB
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

                    when (not $ userIsBot $ messageAuthor m) (handleMessage m)
                    -- handleMessage m
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

                -- log user
                DB.saveUser ctx DB.GuildUser {
                    DB.guildUserId = show userid,
                    DB.guildUserName = userName user,
                    DB.guildUserDiscrim = userDiscrim user 
                }

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
                            Nothing -> sendMessage "I couldn't find anything to run... (did you wrap your code with **```r** ?)"

                   | "ds!search" `isInfixOf` text ->
                    do
                        let searchRegex = mkRegex "ds!search (.*)"
                        case matchRegex searchRegex text of
                            Just query -> do
                                let keywords = splitOn " " (head query)
                                    intersect' x = intersect (rcjTags x) keywords
                                    matches = sortBy (\a b -> (length $ intersect' a) `compare` (length $ intersect' b)) 
                                            $ filter (\rcj -> length (intersect' rcj) > 0) catalog
                                    top = take 5 matches 
                                    matchText = if null top
                                        then "*(No matches.)*"
                                        else concat $ map (\rcj -> 
                                                   ":small_orange_diamond: *" ++ rcjCommand rcj ++ "* **:** `" ++ rcjDescription rcj ++ "`  \n") top 

                                sendMessage $ T.pack $ "**:mag: __Catalog Search Results:__**\n\n" ++ matchText
                            Nothing -> sendMessage "No search query specified."

                   | "ds!run" `isInfixOf` text && isChannelEnabled ->
                    do
                        let channelAndDateRegex = mkRegex "<#([0-9]+)> *: *([^;]+)"
                            channelOnlyRegex = mkRegex "<#([0-9]+)>"

                            parseDate query = (Nothing, Nothing) -- todo TODO parse

                            dataRequests1 =
                                case matchRegex channelAndDateRegex text of
                                    Just matches -> map (\x -> let channel = x !! 0
                                                                   datequery = x !! 1
                                                                in (channel, fst (parseDate datequery), snd (parseDate datequery))) 
                                                    $ chunksOf 2 matches
                                    Nothing -> []

                            commandRegex = mkRegex "ds!run ([a-zA-Z0-9\\/\\-]+)"

                        case matchRegex commandRegex text of
                            Just command -> 
                                if (head command) `elem` (map rcjCommand catalog) -- if command exists in the catalog
                                then mapM_ (\rcj -> -- run that command
                                    if rcjCommand rcj == (head command)
                                    then do
                                        winner <- randomRIO (0, totalrunners - 1) -- todo allocate smarter?
                                        currentTime <- getCurrentTime 

                                        let (channels, from, until) = unzip3 dataRequests1

                                        createJob ctx defaultJob {
                                            jobTitle = (head command) ++ " (" ++ username ++ ")",
                                            jobOwner = winner,
                                            jobChannel = show (messageChannel msg),
                                            jobParameters = RScript {
                                                rsScript = rcjScript rcj,
                                                rsTrusted = ""
                                            },
                                            jobRequestedChannelData = channels,
                                            jobRequestedChannelDataFrom = from,
                                            jobRequestedChannelDataUntil = until 
                                        }

                                        return ()
                                    else return ()) catalog
                                else sendMessage $ T.pack $ "No command named `" ++ (head command) ++ "`."
                            Nothing -> sendMessage "Invalid use of `ds!run`."


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
                                                            DB.pruneMessagesByChannel ctx channelId
                                                            sendMessage . T.pack $ "Disabled indexing and queries in this channel! (& pruned collected data)"
                                                            return ()
                                                    | True -> return ()
                                            else sendMessage "Sorry, you don't have the permissions to do that..."
                                        return ()
                            Nothing -> return ()
                   | True -> return ()


            
               