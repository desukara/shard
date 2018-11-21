module Desukara.ChannelMonitor (
    monitor
) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan

import Discord
import qualified Discord as DS

import DbLib
import DbLib.GuildDataDb.Channels
import qualified DbLib.GuildDataDb.Channels as DB

monitor :: Chan (Either GatewayException Event) -> DbContext -> IO ()
monitor chan ctx = 
    do
        e <- readChan chan

        case e of
            Left _      -> return ()
            Right event -> case event of
                ChannelCreate dc -> updateDb dc (DS.channelGuild dc)
                ChannelUpdate dc -> updateDb dc (DS.channelGuild dc)
                GuildCreate g gi -> mapM_ (\dc -> updateDb dc (guildId g)) (guildChannels gi)
                _ -> return ()

        monitor chan ctx
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