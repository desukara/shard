{-# LANGUAGE OverloadedStrings #-}

module Main where

import Discord
import Discord.Gateway

import Control.Monad (when)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan

import Data.Configurator
import qualified Data.Text as T 
import qualified Data.Text.IO as TIO

import DbLib
import DbLib.CrawlerDb.Jobs

import Desukara.ChannelMonitor
import Desukara.MessageManager
import Desukara.CrawlerSupervisor
import Desukara.Crawler
import Desukara.JobFlusher

main :: IO ()
main = do
  -- load config
  cfg <- load [Required "shard.cfg"]
  let lookup' name default' = lookupDefault default' cfg name

  shardId         <- lookup' "shard.id" (0 :: Int) -- designated shard #
  totalShards     <- lookup' "shard.totalShards" (1 :: Int)
  totalJobRunners <- lookup' "shard.totalJobRunners" (1 :: Int)

  enableCrawler   <- lookup' "shard.crawler.enable" True
  
  -- are we a supervisor shard?
  enableSuper     <- lookup' "shard.supervisor.enable" False
  
  botid           <- lookup' "shard.botId" ""   -- bot discord id


  -- connect to gateway
  token <- T.strip <$> TIO.readFile "auth.secret"
  dis@(_, gateway, _) <- loginRestGatewaySharded (shardId, totalShards) (Auth token)
  chan <- newChan

  sendCommand dis (UpdateStatus (UpdateStatusOpts Nothing UpdateStatusOnline False))

  let eventLoop = do
        e <- nextEvent dis 
        writeChan chan e
        eventLoop 
  forkIO eventLoop 

  -- create db connection 
  e <- connect "shard.cfg"

  case e of
    Left _ -> putStrLn "Could not connect"
    Right ctx -> do
      chan' <- dupChan chan
      forkIO $ monitor chan' ctx

      chan'' <- dupChan chan
      forkIO $ messageManager botid totalJobRunners chan'' dis ctx

      when enableCrawler $ do
            forkIO $ crawler dis ctx shardId 
            return ()

      when enableSuper $ do
            forkIO $ crawlerSupervisor ctx totalShards
            forkIO $ jobFlusher ctx dis
            return ()

      return ()

  let idleLoop = do
        threadDelay (1 * 10^6)
        idleLoop
  idleLoop

