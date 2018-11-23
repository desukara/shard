module Desukara.CrawlerSupervisor (
    crawlerSupervisor
) where

import Control.Concurrent (threadDelay)

import DbLib
import DbLib.CrawlerDb.Jobs
import DbLib.GuildDataDb.Channels

import Data.Time.Clock
import System.Random

type TotalShards = Int

crawlerSupervisor :: DbContext -> TotalShards -> IO ()
crawlerSupervisor ctx totalshards =
    do
        activeChannels <- getActiveChannels ctx

        let channelIds = map channelId activeChannels
        mostRecentJobs <- mapM (getChannelMostRecentJob ctx . channelId) activeChannels
        currentTime <- getCurrentTime

        -- determine which channels need a refresh

        let channelJobMap = channelIds `zip` mostRecentJobs
            noPendingJobs = filter (\(_, x) -> 
                                case x of 
                                    Just job -> (jobStatus job /= Queued) 
                                    Nothing  -> True) channelJobMap
            
            minimumTimeLimit = filter (\(_, x) -> 
                                case x of
                                    Just job -> currentTime `diffUTCTime` (jobCreationDate job)
                                        > 20*60 -- crawl time in seconds
                                        -- > 30
                                    Nothing  -> True) noPendingJobs

            finalJobCandidates = minimumTimeLimit 

        -- todo: sort candidates
       
        if (length finalJobCandidates) > 0
            then putStrLn $ "CrawlerSupervisor: Scheduling " ++ show (length finalJobCandidates) ++ " new job(s)."
            else return ()

        mapM_ (\(channelId, _) -> do
            winner <- randomRIO (0, totalshards - 1) -- todo allocate smarter?
            saveJob ctx 
                Job {   jobId = Nothing,
                        jobStatus = Queued,
                        jobOwner = winner, 
                        jobChannel = channelId
                    }) finalJobCandidates 

        threadDelay (1 * 10^6)
        crawlerSupervisor ctx totalshards
        
