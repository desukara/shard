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

        case e of
            Left _      -> return ()
            Right event -> case event of
                ChannelCreate dc -> updateDb dc (DS.channelGuild dc)
                ChannelUpdate dc -> updateDb dc (DS.channelGuild dc)
                GuildCreate g gi -> do
                    let ds_guildid = guildId g
                        guildid = show $ ds_guildid

                    -- onboarding 
                    complete <- isOnboardingComplete ctx guildid

                    when (not complete) $ do
                        let defaultchannel = DS.channelId 
                                           $ head 
                                           $ filter (\c -> case c of ChannelText _ _ _ _ _ _ _ -> True; _ -> False)
                                           $ guildChannels gi
                        currentTime <- getCurrentTime

                        restCall dis (CreateMessage defaultchannel "" 
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

                        markOnboardingComplete ctx guildid
                        return ()

                    mapM_ (\dc -> updateDb dc ds_guildid) (guildChannels gi)
                GuildDelete (Unavailable ds_guildid) -> pruneMessagesByGuild ctx $ show ds_guildid -- todo: maybe not the wisest to do since guild outages can trigger this event
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