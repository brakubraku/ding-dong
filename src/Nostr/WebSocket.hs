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
    WebSocketAction (..),
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
import Miso.WebSocket hiding (WebSocket(..))
import Nostr.Log
import Nostr.Network
import Nostr.Relay
import Nostr.RelayPool
import Nostr.Response
import Optics
import Prelude hiding (map)
import Data.Time
import Nostr.Event (verifySignature)
import Utils

data WebSocketAction = 
  WebSocketOpen Relay | 
  WebSocketClose Relay Text | 
  WebSocketError Relay Text

connectRelays ::
  NostrNetwork ->
  (WebSocketAction -> action) ->
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

      let markConnected r=  
           modifyMVar_ (nn ^. #relays) $ \rels ->
              pure $ rels & at (r ^. #uri) %~ fmap (Optics.set #connected True)
  
      WS.addEventListener socket "open" $ \_ -> do
        liftIO $ do
          markConnected relay
          _ <- swapMVar socketState 1
          print $ "branko-websocket-open" <> show relay
          sink . sendMsg $ WebSocketOpen relay

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
            case (verifySignature event) of -- TODO: verify hash of event as well
              False -> 
                liftIO
                  . logRelayError relay
                  . pack
                  $ "Failed signature verification of eventId=" <> show event
              True -> 
                case Map.lookup subId subs of
                  Just subscription -> do
                    liftIO $
                      atomically $
                        writeTChan
                          (subscription ^. #responseCh)
                          (EventReceived subId event, relay)
                  Nothing -> do
                    liftIO
                      . logRelayError relay
                      . pack
                      $ "SubId="
                        <> show subId
                        <> " not found in responseChannels. Event received="
                        <> show event

          Just (Nostr.Response.EOSE subId) -> do
            liftIO . runReaderT (changeState subId relay (fmap . const $ Nostr.Network.EOSE)) $ nn
          Just (Nostr.Response.OK eid True _) -> do
            liftIO . flip runReaderT nn $ setResultSuccess eid relay
          Just (Nostr.Response.OK eid False reason) -> do
            liftIO . flip runReaderT nn $ setResultError (fromMaybe "" reason) eid relay
          _ -> do 
               liftIO . logRelayError relay . pack $ "Could not decode server response: " <> show msg

      WS.addEventListener socket "close" $ \e -> do
        code <- codeToCloseCode <$> WS.code e
        reason <- WS.reason e
        clean <- WS.wasClean e
        liftIO . sink . sendMsg $ (WebSocketClose relay $ decodeError code clean reason)
        liftIO . print $ "closed connection " <> show relay <> " because " <> show code <> show reason <> show clean
        status <- WS.socketState socket
        _ <- liftIO . swapMVar socketState $ status
        now <- liftIO getCurrentTime
        when (status == 3) $
          unless (code == CLOSE_NORMAL || code == CLOSE_NO_STATUS) $ do
            liftIO . print $ show now <> ":xxy-reconnecting " <> show relay
            let diff = (round $ diffUTCTime now lastReconnect)
            case (recnt > 3, diff > 1) of -- TODO: take time into account?
              (True,_) -> do 
                liftIO . sleep . Seconds $ 5
                conRelay (0,now) relay 
              (False, _) -> do 
                liftIO . sleep . Seconds $ 0.5 
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
            liftIO . sink . sendMsg $ (WebSocketError relay mempty)
            liftIO . print $ "branko-subId-websocket-error"
          else do
            Just d <- fromJSVal d'
            liftIO . print $ "branko-subId-websocket-error:" <> show d
            liftIO . sink . sendMsg $ (WebSocketError relay d)

      rc <- liftIO . atomically . dupTChan $ (nn ^. #requestCh)
      -- listen for requests to send
      let doLoop =
            do
              status <- liftIO $ readMVar socketState
              case status of
                0 -> do
                  -- not ready yet
                  liftIO . sleep . Seconds $ 0.1
                  doLoop
                1 -> do
                  -- ready
                  request <- liftIO . atomically . readTChan $ rc
                  sendJson' socket request -- TODO: if the socket is porked at this point this will pork up
                  doLoop
                2 -> markAllError relay "Relay closed connection"
                3 -> markAllError relay "Relay closed connection"
                _ -> markAllError relay "Error received from relay"
      doLoop
      WS.close socket

    markAllError relay eText = do 
      liftIO . runNostr nn $ do 
        markDisconnected relay
        changeStateForAllSubs relay (fmap . const $ Nostr.Network.Error eText)

markDisconnected :: Relay -> NostrNetworkT ()
markDisconnected r = do 
  nn <- ask 
  liftIO . modifyMVar_ (nn ^. #relays) $ 
    \rels -> do 
        pure $ rels & at (r ^. #uri) %~ fmap (Optics.set #connected False)

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

decodeError code clean reason = "Connection closed" -- TODO