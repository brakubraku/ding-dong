{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE FlexibleContexts     #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Subscription.WebSocket
-- Copyright   :  (C) 2016-2018 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Nostr.WebSocket
  ( -- * Types
    WebSocket   (..)
  , URL         (..)
  , Protocols   (..)
  , SocketState (..)
  , CloseCode   (..)
  , WasClean    (..)
  , Reason      (..)
    -- * Subscription
  , connectRelays
  ) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Maybe
import           GHCJS.Marshal
import           GHCJS.Foreign
import           GHCJS.Types ()
import           Prelude hiding (map)

import           Miso.Effect (Sub)
import           Miso.FFI
import           Miso.FFI.WebSocket (Socket)
import qualified Miso.FFI.WebSocket as WS
import           Miso.String
import           Miso.WebSocket 

import Language.Javascript.JSaddle.Value (valToStr)
import Language.Javascript.JSaddle.String

import Nostr.Response
import Nostr.Relay
import Nostr.RelayPool

import Nostr.Network
import Nostr.Log
import qualified Data.Map as Map
import Optics

import Data.Text.Encoding (encodeUtf8)
import Data.ByteString (fromStrict)
import Nostr.Request

-- data WebSocketEvent = WebSocketOpen | WebSocketError String 

connectRelays
  :: 
  NostrNetwork 
  -> (WebSocket () -> action)
  -> Sub action
connectRelays nn@(NostrNetwork{..}) sendMsg sink = do
  -- connect a relay
 relays <- liftIO $ readMVar (nn ^. #relays)
 mapM_ (forkJSM . conRelay) relays
 
 where
  conRelay :: Relay -> JSM ()
  conRelay relay = do 
    -- TODO: maybe I need to store the socket reference somewhere or else it gets GCd?
    socket <- createWebSocket (relay ^. #uri) [] 
    
    WS.addEventListener socket "open" $ \_ -> do
      liftIO $ do 
        modifyMVar_ (nn ^. #relays) $ \rels -> 
         pure $ rels & at (relay ^. #uri) %~ fmap (\r -> r {connected = True})
        sink . sendMsg $ WebSocketOpen
    
    WS.addEventListener socket "message" $ \v -> do
      msg <- valToStr =<< WS.data' v
      resp <- pure -- TODO: I am going to hell for this 
                . decode @Response       
                . fromStrict 
                -- . fromRight "" 
                . encodeUtf8
                -- . decodeBase16Untyped
                . strToText $ msg
      -- liftIO . putStrLn $ "Raw-dog:" <> T.unpack (strToText $ msg)
      -- fn <- liftIO $ getEntropy 8
      -- liftIO . writeFile ("./events/" ++ BS.unpack fn) $ T.unpack . strToText $ msg
      case resp of 
        Just (EventReceived subId event) -> do
            -- liftIO . putStrLn $ "branko-received-event:" <> show (event)
            subs <- liftIO . readMVar $ (nn ^. #subscriptions)
            case Map.lookup subId subs of
              Just subscription -> do
                -- normalize event's tags to latest NIP10
                -- TODO: reintroduce this below if neccessarry
                -- let normalizedEvent = normalizeTags event
                let normalizedEvent = event
                liftIO $ atomically $ writeTChan (subscription ^. #responseCh) (EventReceived subId normalizedEvent, relay)
              Nothing ->
                liftIO . logRelayError  relay . pack $ "SubId=" <> show subId <> " not found in responseChannels. Event received=" <> show event 
        Just (Nostr.Response.EOSE subId) -> do 
          liftIO . runReaderT (changeState subId relay Nostr.Network.EOSE) $ nn
        _ -> liftIO . logRelayError  relay . pack $ "Could not decode server response: " <> show msg
    
    WS.addEventListener socket "close" $ \e -> do
      code <- codeToCloseCode <$> WS.code e
      reason <- WS.reason e
      clean <- WS.wasClean e
      liftIO . sink . sendMsg $ (WebSocketClose code clean reason)
      status <- WS.socketState socket
      liftIO . logRelayError  relay . pack $ "branko-websocket closed: " <> show reason
      when (status == 3) $ 
        unless (code == CLOSE_NORMAL) $
          -- websocketConnect chan [r] sink
          connectRelays nn sendMsg sink 

    WS.addEventListener socket "error" $ \v -> do
      d' <- WS.data' v
#ifndef ghcjs_HOST_OS
      undef <- ghcjsPure (isUndefined d')
#else
      let undef = isUndefined d'
#endif
      if undef
        then do
          liftIO . sink . sendMsg $ (WebSocketError mempty)
        else do
          Just d <- fromJSVal d'
          liftIO . logRelayError  relay . pack $ "branko-Websocket error" <> show d
          liftIO . sink . sendMsg $ (WebSocketError d)

    -- listen for requests to send 
    rc <- liftIO . atomically . cloneTChan $ (nn ^. #requestCh)
    void . forever $ 
      do
        request <- liftIO . atomically . readTChan $ rc
        case request of 
          Subscribe (Subscription _ subId) -> 
            liftIO . runReaderT (changeState subId relay Nostr.Network.Running) $ nn
          _ -> pure ()
        -- TODO: get rid of this
        sendJson' socket request

sendJson' :: ToJSON json => Socket -> json -> JSM ()
sendJson' socket m = do 
  -- liftIO . putStrLn $ "brankoSending: " <> (show . toJSON $ m)
  WS.send socket =<< stringify m

createWebSocket :: MisoString -> [MisoString] -> JSM Socket
{-# INLINE createWebSocket #-}
createWebSocket url' protocols =
  WS.create url' =<< toJSVal protocols

codeToCloseCode :: Int -> CloseCode
codeToCloseCode = go
  where
    go 1000 = CLOSE_NORMAL
    go 1001 = CLOSE_GOING_AWAY
    go 1002 = CLOSE_PROTOCOL_ERROR
    go 1003 = CLOSE_UNSUPPORTED
    go 1005 = CLOSE_NO_STATUS
    go 1006 = CLOSE_ABNORMAL
    go 1007 = Unsupported_Data
    go 1008 = Policy_Violation
    go 1009 = CLOSE_TOO_LARGE
    go 1010 = Missing_Extension
    go 1011 = Internal_Error
    go 1012 = Service_Restart
    go 1013 = Try_Again_Later
    go 1015 = TLS_Handshake
    go n    = OtherCode n
