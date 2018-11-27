{-# LANGUAGE OverloadedStrings #-}

module Desukara.ChannelMonitor (
    monitor
) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan

import Discord
import qualified Discord as DS

import DbLib
import DbLib.GuildDataDb.Channels
import DbLib.GuildDataDb.Guilds
import DbLib.GuildDataDb.Messages 
import qualified DbLib.GuildDataDb.Channels as DB

import Control.Monad (when)
import Data.Time.Clock
import Data.List (intercalate)

type DisCtx = (RestChan, Gateway, [ThreadIdType])


monitor :: Chan (Either GatewayException Event) -> DisCtx -> DbContext -> IO ()
monitor chan dis ctx = 
    do
        e <- readChan chan

        print e

        case e of
            Left _      -> return ()
            Right event -> case event of
                ChannelCreate dc -> updateDb dc (DS.channelGuild dc)
                ChannelUpdate dc -> updateDb dc (DS.channelGuild dc)
                GuildCreate g gi -> mapM_ (\dc -> updateDb dc (guildId g)) (guildChannels gi)
                GuildDelete (Unavailable ds_guildid) -> 
                    -- todo prune jobs by guild
                    pruneMessagesByGuild ctx $ show ds_guildid -- todo: maybe not the wisest to do since guild outages can trigger this event
                _ -> return ()

        monitor chan dis ctx
    where
        updateDb dc guild = case dc of
            ChannelText _ _ _ _ _ _ _ ->
                DB.saveChannel ctx Channel {
                    DB.channelId = show $ DS.channelId dc,
                    -- DB.channelGuild = show $ DS.channelGuild dc,
                    DB.channelGuild = show $ guild,
                    DB.channelName = DS.channelName dc,
                    DB.channelEnabled = Nothing,
                    DB.channelPriority = Nothing
                }
            _ -> return ()