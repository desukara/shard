module Main where

import Discord
import Discord.Gateway

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan

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
  token <- T.strip <$> TIO.readFile "auth.secret"
  dis@(_, gateway, _) <- loginRestGateway (Auth token)
  chan <- newChan

  sendCommand dis (UpdateStatus (UpdateStatusOpts Nothing UpdateStatusOnline False))

  let eventLoop = do
        e <- nextEvent dis 
        writeChan chan e
        eventLoop 
  forkIO eventLoop 

  e <- connect (T.pack "db.cfg")
  case e of
    Left _ -> putStrLn "Could not connect"
    Right ctx -> do
      chan' <- dupChan chan
      forkIO $ monitor chan' ctx

      chan'' <- dupChan chan
      forkIO $ messageManager chan'' dis ctx

      forkIO $ crawler dis ctx 0
      forkIO $ crawlerSupervisor ctx

      forkIO $ jobFlusher ctx dis

      return ()

  let idleLoop = do
        threadDelay (1 * 10^6)
        idleLoop
  idleLoop

