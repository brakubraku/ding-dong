{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Nostr.WebSocket
  ( -- * Types
    WebSocket (..),
    URL (..),
    Protocols (..),
    SocketState (..),
    CloseCode (..),
    WasClean (..),
    Reason (..),

    -- * Subscription
    connectRelays,
  )
where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Reader
import Data.Aeson
import Data.ByteString (fromStrict)
import qualified Data.Map as Map
import Data.Maybe
import Data.Text.Encoding (encodeUtf8)
import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Types ()
import Language.Javascript.JSaddle.String
import Language.Javascript.JSaddle.Value (valToStr)
import Miso.Effect (Sub)
import Miso.FFI
import Miso.FFI.WebSocket (Socket)
import qualified Miso.FFI.WebSocket as WS
import Miso.String
import Miso.WebSocket
import Nostr.Log
import Nostr.Network
import Nostr.Relay
import Nostr.RelayPool
import Nostr.Response
import Optics
import Prelude hiding (map)
import Data.Time

connectRelays ::
  NostrNetwork ->
  (WebSocket () -> action) ->
  Sub action
connectRelays nn sendMsg sink = do
  -- connect a relay
  relays <- liftIO $ readMVar (nn ^. #relays)
  now <- liftIO getCurrentTime
  mapM_ (forkJSM . conRelay (0, now)) relays
  where
    conRelay :: (Int, UTCTime) -> Relay -> JSM ()
    conRelay (recnt, lastReconnect) relay = do
      socket <- createWebSocket (relay ^. #uri) []
      socketState <- liftIO $ newMVar 0

      WS.addEventListener socket "open" $ \_ -> do
        liftIO $ do
          modifyMVar_ (nn ^. #relays) $ \rels ->
            pure $ rels & at (relay ^. #uri) %~ fmap (\r -> r {connected = True})
          _ <- swapMVar socketState 1
          print $ "branko-websocket-open" <> show relay
          sink . sendMsg $ WebSocketOpen

      WS.addEventListener socket "message" $ \v -> do
        msg <- valToStr =<< WS.data' v
        resp <-
          pure
            . decode @Response
            . fromStrict
            . encodeUtf8
            . strToText
            $ msg
        case resp of
          Just (EventReceived subId event) -> do
            subs <- liftIO . readMVar $ (nn ^. #subscriptions)
            case Map.lookup subId subs of
              Just subscription -> do
                liftIO $
                  atomically $
                    writeTChan
                      (subscription ^. #responseCh)
                      (EventReceived subId event, relay)
              Nothing ->
                liftIO
                  . logRelayError relay
                  . pack
                  $ "SubId="
                    <> show subId
                    <> " not found in responseChannels. Event received="
                    <> show event
          Just (Nostr.Response.EOSE subId) -> do
            liftIO . runReaderT (changeState subId relay (fmap . const $ Nostr.Network.EOSE)) $ nn
          _ -> liftIO . logRelayError relay . pack $ "Could not decode server response: " <> show msg

      WS.addEventListener socket "close" $ \e -> do
        code <- codeToCloseCode <$> WS.code e
        reason <- WS.reason e
        clean <- WS.wasClean e
        liftIO . sink . sendMsg $ (WebSocketClose code clean reason)
        liftIO . print $ "closed connection " <> show relay <> " because " <> show code <> show reason <> show clean
        status <- WS.socketState socket
        _ <- liftIO . swapMVar socketState $ status
        now <- liftIO getCurrentTime
        when (status == 3) $
          unless (code == CLOSE_NORMAL) $ do
            let diff = (round $ diffUTCTime now lastReconnect)
            case (recnt > 3, diff > 1) of -- TODO: take time into account?
              (True,_) -> do 
                liftIO . threadDelay $ 10 ^ 6 * 5 -- wait 5 secs to reconnect 
                conRelay (0,now) relay 
              (False, _) -> do 
                liftIO . threadDelay $ (10 ^ 5) * 5  -- wait 1/2 sec to reconnect 
                conRelay (recnt+1,now) relay 

      WS.addEventListener socket "error" $ \v -> do
        _ <- liftIO . swapMVar socketState $ 4 -- TODO: 4 means error 
        d' <- WS.data' v
#ifndef ghcjs_HOST_OS
        undef <- ghcjsPure (isUndefined d')
#else
        let undef = isUndefined d'
#endif
        if undef
          then do
            liftIO . sink . sendMsg $ (WebSocketError mempty)
            liftIO . print $ "branko-subId-websocket-error"
          else do
            Just d <- fromJSVal d'
            liftIO . print $ "branko-subId-websocket-error:" <> show d
            liftIO . sink . sendMsg $ (WebSocketError d)

      rc <- liftIO . atomically . cloneTChan $ (nn ^. #requestCh)
      -- listen for requests to send
      let doLoop =
            do
              status <- liftIO $ readMVar socketState
              liftIO . print $ "branko-puppy:" <> show relay <> " running" <> " status=" <> show status 
              case status of
                0 -> do
                  -- not ready yet
                  liftIO . threadDelay $ 10 ^ 5
                  doLoop
                1 -> do
                  -- ready
                  request <- liftIO . atomically . readTChan $ rc
                  sendJson' socket request -- TODO: if the socket is porked at this point this will pork up
                  doLoop
                2 -> pure () -- exit but don't change subscription status, in case reconnect happens
                3 -> pure () 
                _ -> do 
                  -- mark all non-EOSE subscriptions on this Relay as errored
                  let changeToError st
                        | st /= Nostr.Network.EOSE = Nostr.Network.Error "Error" -- TODO: more descriptive error
                        | otherwise = st
                  liftIO . runReaderT (changeStateForAllSubs relay (fmap changeToError)) $ nn
      doLoop

sendJson' :: (ToJSON json) => Socket -> json -> JSM ()
sendJson' socket m = do
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
    go n = OtherCode n
