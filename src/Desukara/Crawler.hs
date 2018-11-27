module Desukara.Crawler (
    crawler
) where

import DbLib
import DbLib.CrawlerDb.Jobs
import DbLib.GuildDataDb.Messages
import qualified DbLib.GuildDataDb.Messages as DB

import Discord
import qualified Discord as DS

import Control.Concurrent (threadDelay)
import Data.Time.Clock
import Data.Maybe (fromJust)
import qualified Data.Text as T

type CrawlerNumber = Int

crawler :: (RestChan, Gateway, [ThreadIdType]) -> DbContext -> CrawlerNumber -> IO () 
crawler dis ctx no =
    do
        result <- getAvailableJobs ctx no

        case result of
            []    -> return () -- nothing to do...
            jobs  -> do
                    putStrLn $ "Crawler #" ++ show no ++ ": Starting job "

                    let selectedJob = head jobs
                        channel = jobChannel selectedJob

                    putStrLn $ "Crawler #" ++ show no ++ ": Starting job " ++ show selectedJob ++ "..."

                    -- grab last indexed message from channel
                    maybeIndexed <- getChannelLastIndexedMessage ctx channel

                    -- grab most recent message from channel
                    maybeRecent <- restCall dis (GetChannel ((fromInteger . read) channel))

                    putStrLn $ "Most recent message (from): " ++ show maybeRecent
                    putStrLn $ "Last indexed (until): " ++ show maybeIndexed
                    putStrLn $ "Starting crawl..."

                    case maybeRecent of
                        Left e -> do
                            -- fail!
                            saveJob ctx $ selectedJob { jobStatus = Failed }
                            putStrLn $ "Craw failed! " ++ show e
                        Right dc -> 
                            do 
                                let mostRecentMsg = show $ fromJust $ channelLastMessage dc
                                    indexUntilMsg = fmap DB.messageId maybeIndexed 
                                    guild = show $ channelGuild dc

                                success <- crawlLoop dis ctx
                                                     channel guild mostRecentMsg indexUntilMsg

                                putStrLn $ "Crawl finished. Success: " ++ show success

                                saveJob ctx $ selectedJob { jobStatus = if success then Finished else Failed }

        threadDelay (1 * 10^6)
        crawler dis ctx no

crawlLoop :: (RestChan, Gateway, [ThreadIdType]) -> DbContext 
          -> String -> String -> String -> Maybe String -> IO Bool
crawlLoop dis ctx 
          chan guild from until =
    do
        let ds_chan = fromInteger $ (read chan :: Integer)
            ds_from = fromInteger $ (read from :: Integer)

        maybeMsgs <- restCall dis (GetChannelMessages ds_chan (100, BeforeMessage ds_from))

        case maybeMsgs of
            Left _      -> return False -- comms failure, abort crawl!
            Right msgs  -> do
                -- save messages
                mapM_ (\m -> do
                    currentTime <- getCurrentTime 
                   
                    let mentions = map (\u -> show $ userId u) (DS.messageMentions m)

                    saveMessage ctx DB.Message {
                        DB.messageId = show $ DS.messageId m,
                        DB.messageLastIndexed = currentTime,
                        DB.messageChannel = chan, 
                        DB.messageGuild = guild,
                        DB.messageAuthor = show $ userId $ DS.messageAuthor m,
                        DB.messageContents = T.unpack $ DS.messageText m, 
                        DB.messageTimestamp = DS.messageTimestamp m, 
                        DB.messageEditedTimestamp = DS.messageEdited m, 
                        DB.messageTTS = DS.messageTts m, 
                        DB.messageMentions = map (\u -> show $ userId u) (DS.messageMentions m),
                        DB.messageMentionsEveryone = DS.messageEveryone m,
                        DB.messageMentionedRoles = map show (DS.messageMentionRoles m), 
                        DB.messageAttachments = map DS.attachmentUrl (DS.messageAttachments m),
                        DB.messageContainsEmbeds = length (DS.messageEmbeds m) > 0,
                        DB.messageIsPinned = DS.messagePinned m 
                    }
                    -- threadDelay (round $ (0.3 * 10^6))) msgs
                    return ()) msgs

                -- recursion logic
                -- if we:
                -- A. reach the message specified by `until`
                -- or
                -- B. Discord gives us less than 100 messages
                -- then we consider the crawl to be finished and
                -- set the `terminate` flag
                let msgIds = map (\m -> show $ DS.messageId m) msgs
                    terminate = case until of
                                    Just x -> x `elem` msgIds
                                    Nothing -> False
                                ||
                                length msgIds < 100

                if terminate
                    then return True
                    else do
                        threadDelay (2 * 10^6)
                        crawlLoop dis ctx chan guild (last msgIds) until

