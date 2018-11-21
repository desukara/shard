{-# LANGUAGE OverloadedStrings #-}

module Desukara.JobFlusher (
    jobFlusher
) where

import DbLib
import DbLib.JobManagerDb.Jobs

import Discord

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan
import Data.Time.Clock

type DisCtx = (RestChan, Gateway, [ThreadIdType])

jobFlusher :: DbContext -> DisCtx -> IO ()
jobFlusher ctx dis =
    do
        dirtyJobs <- getDirtyJobs ctx

        let foreachJob job = 
                do
                currentTime <- getCurrentTime

                let id = jobId job 
                    channel = jobChannel job 
                    status = jobStatus job
                    output = jobOutput job
                    ds_channel = fromInteger $ (read channel :: Integer)
                   
                    inlineFields = map (\(x, y) -> Field x y True) 
                        $ zip (inlineTitles output) (inlineText output)
                    imageFields = map (\x -> Image x "" 500 500) (imageUrls output)
                    thumbnailField = Thumbnail (case status of
                        Queued -> "https://i.imgur.com/cTewsfV.png"
                        Running -> ""
                        Finished -> ""
                        Failed -> "") "" 50 50
                    footerText = case footer output of
                        Just x -> x
                        Nothing -> "" --todo
                    footerField = [Footer footerText "" ""] --todo

                    embed = Embed {
                        embedTitle = (case status of
                                        Queued -> "QUEUED: "
                                        Running -> "RUNNING: "
                                        Finished -> "FINISHED: "
                                        Failed -> "FAILED: ") ++ jobTitle job,
                        embedType = "rich",
                        embedDesc = description output,
                        embedUrl = "",
                        embedTime = currentTime,
                        embedColor = (case status of
                                        Queued -> 13092807
                                        Running -> 6353009
                                        Finished -> 15073535
                                        Failed -> 16711680),
                        embedFields = [thumbnailField] ++ inlineFields ++ imageFields ++ footerField
                    }

                case jobDiscordMessage job of
                    Nothing -> do 
                        result <- restCall dis (CreateMessage ds_channel "" (Just embed))
                        case result of
                            Left e -> putStrLn $ "Error while flushing: " ++ show e
                            Right msg -> do 
                                setJobDiscordMsg (show $ messageId msg) ctx id
                                setJobDirty False ctx id
                    Just msgid -> do
                        let ds_msgid = fromInteger $ (read msgid :: Integer)
                        result <- restCall dis (EditMessage (ds_channel, ds_msgid) "" (Just embed))
                        case result of
                            Left e -> putStrLn $ "Error while flushing: " ++ show e
                            Right msg -> setJobDirty False ctx id
                        
        mapM foreachJob dirtyJobs

        threadDelay (1 * 10^6)
        jobFlusher ctx dis