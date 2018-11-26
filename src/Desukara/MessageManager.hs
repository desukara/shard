{-# LANGUAGE OverloadedStrings, MultiWayIf, FlexibleContexts #-}

module Desukara.MessageManager (
    messageManager
) where

import Control.Monad (when)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan
import Data.Bits
import Data.Ord (compare)
import Data.List (sortBy, isInfixOf, intersect, intercalate)
import Data.List.Split (splitOn, chunksOf)
import Data.Maybe (fromJust, isJust)
import Data.Time
import Data.Time.Clock
import Data.Time.Format
import qualified Data.Text as T
import Text.Regex.TDFA
import Text.Read (readMaybe)
import System.Random

import Discord
import qualified Discord as DS

import Desukara.RCatalog

import DbLib
import DbLib.GuildDataDb.Channels
import DbLib.GuildDataDb.Guilds
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
        getCaptures [] = []
        getCaptures regex = concatMap tail (regex :: [[String]])

        match' a b = match (makeRegexOpts defaultCompOpt{multiline=False} defaultExecOpt (a :: String)) b

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

                currentTime <- getCurrentTime

                if | "ds!eval" `isInfixOf` text && isChannelEnabled -> 
                        case getCaptures $ ("```r(.*)```" `match'` text) of
                            [] -> sendMessage "I couldn't find anything to run... (did you wrap your code with **```r** ?)"
                            matches -> do
                                let code = head matches 

                                putStrLn $ show matches

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
                                    jobRequestedChannelData = [ show (messageChannel msg) ], -- todo
                                    jobRequestedChannelDataFrom = [ Nothing ],
                                    jobRequestedChannelDataUntil = [ Nothing ]
                            } 

                   | "ds!search" `isInfixOf` text ->
                        case getCaptures $ ("ds!search (.*)" `match'` text) of
                            [] -> sendMessage "No search query specified."
                            query -> do
                                let keywords = splitOn " " (head query)
                                    intersect' x = intersect (rcjTags x) keywords
                                    matches = sortBy (\a b -> (length $ intersect' a) `compare` (length $ intersect' b)) 
                                            $ filter (\rcj -> length (intersect' rcj) > 0) catalog
                                    top = take 5 matches 
                                    matchText = if null top
                                        then "*(No matches.)*"
                                        else concatMap (\rcj -> 
                                                    ":small_orange_diamond: *" ++ rcjCommand rcj ++ "* **:** `" ++ rcjDescription rcj ++ "`  \n") top 

                                sendMessage $ T.pack $ "**:mag: __Catalog Search Results:__**\n\n" ++ matchText

                   | "ds!run" `isInfixOf` text && isChannelEnabled ->
                    do
                        let channelAndDateMatches =  ("<#([0-9]+)> *: *([^;]+)" `match'` text)
                            channelOnlyRegex = getCaptures $ ("<#([0-9]+)>" `match'` text)
                            usersRegex = getCaptures $ ("<@([0-9]+)>" `match'` text)

                            parseDate query =
                                let matchRange = getCaptures $
                                        match' "([0-9]{1,2})\\/([0-9]{1,2})\\/([0-9]{2}).*-.*([0-9]{1,2})\\/([0-9]{1,2})\\/([0-9]{2})" query
                                    matchPastN = getCaptures $
                                        match' "past (.+) (.+)" query
                                in if 
                                    | not . null $ matchRange ->
                                        let fm = readMaybe $ matchRange !! 0
                                            fd = readMaybe $ matchRange !! 1 
                                            fy = readMaybe $ matchRange !! 2

                                            um = readMaybe $ matchRange !! 3
                                            ud = readMaybe $ matchRange !! 4 
                                            uy = readMaybe $ matchRange !! 5

                                            fromDate = 
                                                if isJust fm && isJust fd && isJust fy
                                                then fmap (\x -> UTCTime x 0) $ fromGregorianValid 
                                                        (2000 + fromJust fy)
                                                        (fromJust fm)
                                                        (fromJust fd) 
                                                else Nothing
                                            untilDate = 
                                                if isJust um && isJust ud && isJust uy
                                                then fmap (\x -> UTCTime x 0) $ fromGregorianValid 
                                                        (2000 + fromJust uy)
                                                        (fromJust um)
                                                        (fromJust ud) 
                                                else Nothing
                                        in (fromDate, untilDate)

                                    | not . null $ matchPastN ->
                                        let maybeQuantity = readMaybe $ matchPastN !! 0
                                            units = matchPastN !! 1
                                        in if isJust maybeQuantity 
                                           then let quantity = fromJust maybeQuantity

                                                    second = 1
                                                    minute = second * 60
                                                    hour = minute * 60
                                                    day = hour * 24
                                                    week = day * 7
                                                    month = day * 31
                                                    year = month * 12

                                                    diff = fromIntegral $ case units of
                                                        "minute"    -> quantity * minute
                                                        "minutes"   -> quantity * minute
                                                        "hour"      -> quantity * hour
                                                        "hours"     -> quantity * hour
                                                        "day"       -> quantity * day
                                                        "days"      -> quantity * day
                                                        "week"      -> quantity * week
                                                        "weeks"     -> quantity * week
                                                        "month"     -> quantity * month
                                                        "months"    -> quantity * month
                                                        "year"      -> quantity * year
                                                        "years"     -> quantity * year
                                                        _           -> year

                                                    fromDate = addUTCTime (-1 * diff) currentTime
                                                in (Just fromDate, Nothing) -- todo
                                           else (Nothing, Nothing)
                                        
                                    | True -> (Nothing, Nothing)

                            dataRequests1 = map (\x -> let  channel = x !! 1
                                                            datequery = x !! 2
                                                                in (channel, fst (parseDate datequery), snd (parseDate datequery))) 
                                          $ channelAndDateMatches
                            dataRequests1Channels = map (\(chan, _, _) -> chan) dataRequests1

                            dataRequests2Partial = filter (\(chan, _, _) -> chan `notElem` dataRequests1Channels) 
                                                 $ map (\chan -> (chan, Nothing, Nothing)) channelOnlyRegex

                        case getCaptures $ ("ds!run ([a-zA-Z0-9\\/\\-]+)" `match'` text) of
                            [] -> sendMessage "Invalid use of `ds!run`."
                            command -> 
                                if (head command) `elem` (map rcjCommand catalog) -- if command exists in the catalog
                                then mapM_ (\rcj -> -- run that command
                                    if rcjCommand rcj == (head command)
                                    then do
                                        winner <- randomRIO (0, totalrunners - 1) -- todo allocate smarter?
                                        currentTime <- getCurrentTime 

                                        let (channels, from, until) = unzip3 (dataRequests1 ++ dataRequests2Partial)

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
                                            jobRequestedChannelDataUntil = until,
                                            jobMentionedUsers = usersRegex
                                        }

                                        return ()
                                    else return ()) catalog
                                else sendMessage $ T.pack $ "No command named `" ++ (head command) ++ "`."

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
                                                    | "ds@about" `isInfixOf` text ->
                                                        do
                                                            currentTime <- getCurrentTime

                                                            restCall dis (CreateMessage ds_channel "" 
                                                                $ Just Embed {
                                                                    embedTitle = "Hello all!",
                                                                    embedType = "rich",
                                                                    embedDesc = intercalate "  \n" 
                                                                    [   "**Thanks for the invite!**"
                                                                    ,   "I aggregate messages and let you generate graphs and statistics about them!"
                                                                    ,   "Before we get started, here are a couple things you should know:"
                                                                    ,   ""
                                                                    ,   "- I won't aggregate messages from channels unless you *explicitly enable them*. Type `ds@enableChannel` "
                                                                    ++  "in the appropriate channel to *enable aggregation of all messages available* in the channel "
                                                                    ++  "*(in accordance with Section 2.5d of the Discord Developer ToS)*. This is required to use any data-related "
                                                                    ++  "commands in a channel."
                                                                    ,   ""
                                                                    ,   "- I can take ~10 to 20 minutes to completely aggregate messages from a channel after enabling it. "
                                                                    ++  "I'll then index a channel's new messages every 20 minutes. "
                                                                    ++  "It's wise to wait at least 10 minutes after enabling a channel before running a query in order to "
                                                                    ++  "get accurate reports."
                                                                    ,   ""
                                                                    ,   "- I'll aggregate *all messages available* from enabled channels for *as long as the the channel is enabled* "
                                                                    ++  "in order to generate accurate reports *(with exceptions as defined by Section 2.4 & 2.5 of the Discord Developer ToS)*. "
                                                                    ++  "Channels disabled using `ds@disableChannel` will have their data pruned as soon as possible."
                                                                    ,   ""
                                                                    ,   "- I'll prune all messages from this guild as soon as possible if I leave the guild "
                                                                    ++  "*(in accordance with Section 2.4 of the Discord Developer ToS)*."
                                                                    ,   "" 
                                                                    ,   "That's all! To get started, type `ds!help` to get a list of commands available. Thanks, and have fun!"
                                                                    ,   ""
                                                                    ,   "*~ Sleepy, the 2nd Class Angel*"
                                                                    ],
                                                                    embedUrl = "",
                                                                    embedTime = currentTime,
                                                                    embedColor = 16567412,
                                                                    embedFields = []
                                                                })

                                                            markOnboardingComplete ctx (show $ guildId)
                                                            return()
                                                    | True -> return ()
                                            else sendMessage "Sorry, you don't have the permissions to do that..."
                                        return ()
                            Nothing -> return ()

                   | "ds!help" `isInfixOf` text -> do
                        restCall dis (CreateMessage ds_channel "" 
                            $ Just Embed {
                                embedTitle = "Desukara - Available Commands",
                                embedType = "rich",
                                embedDesc = "",
                                embedUrl = "",
                                embedTime = currentTime,
                                embedColor = 16567412,
                                embedFields = [
                                    Field "**ds!help**" "View this message." True,
                                    Field "**ds!search** *[keywords, ...]*" 
                                       (   "Search for a command in the catalog using keywords.  \n"
                                        ++ "*e.g.* `ds!search graph activity`")
                                       False,
                                    Field "**ds!run** *command [@user, ...] [#channel: optionalDateRange; ...]*" 
                                       (   "Run a command from the catalog with data from the given channels (and an optional date range).  \n"
                                        ++ "*e.g.* `ds!run graphChannelActivity #general`  \n"
                                        ++ "`ds!run graphChannelActivity #botspam: past 5 days; #memes: 12/28/18-01/01/19`  \n"
                                        ++ "`ds!run sentimentAnalysis @cat protector 40,000#0258 #general: past 1 year`")
                                       True,
                                    -- Field "**ds!eval** *\\`\\`\\`R [rCode] \\`\\`\\`*" 
                                    --    (   "Evaluate a block of R code. Useful for testing additions to the catalog.  \n"
                                    --    ++  "Further documentation planned." ) True,
                                    Field "**ds@enableChannel** (admin)" "Enable data aggregation from the channel." True,
                                    Field "**ds@disableChannel** (admin)" "Disable data aggregation from the channel and mark data for deletion." True,
                                    Field "**ds@about** (admin)" "View the about message." True
                                ]
                            })
                        return ()
                   | True -> return ()


            
               