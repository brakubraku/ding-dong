{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Nostr.Event (verifySignature, validateEventId)
import Utils
import Debug.Trace (traceM)

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
      isReconnectingMVar <- liftIO $ newMVar False
      socket <- createWebSocket (relay ^. #uri) []

      let 
        reconnect =
          do
            isReconnecting <- liftIO $ readMVar isReconnectingMVar
            unless isReconnecting $ do
              liftIO $ markIsConnected False relay
              liftIO $ modifyMVar_ isReconnectingMVar (const . pure $ True)
              now <- liftIO getCurrentTime
              liftIO . print $ show now <> "reconnecting " <> show relay
              let diff = (round $ diffUTCTime now lastReconnect)
              case (recnt > 3, diff > 1) of -- TODO: take time into account?
                (True, _) -> do
                  liftIO . sleep . Seconds $ 5
                  conRelay (0, now) relay
                (False, _) -> do
                  liftIO . sleep . Seconds $ 0.5
                  conRelay (recnt + 1, now) relay

      WS.addEventListener socket "open" $ \_ -> do
        liftIO $ do
          markIsConnected True relay
          sink . sendMsg $ WebSocketOpen relay

      WS.addEventListener socket "message" $ \v -> do
        msg <- valToStr =<< WS.data' v
        resp <-
          pure
            . eitherDecode @Response
            . fromStrict
            . encodeUtf8
            . strToText
            $ msg
        case resp of
          Right (EventReceived subId event) -> do
            subs <- liftIO . readMVar $ (nn ^. #subscriptions)
            when (not $ validateEventId event) $ 
             traceM ("branko-failed-hash-validation: " <> show msg)
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

          Right (Nostr.Response.EOSE subId) -> do
            liftIO . runReaderT (changeState subId relay (fmap . const $ Nostr.Network.EOSE)) $ nn
          Right (Nostr.Response.OK eid True _) -> do
            liftIO . flip runReaderT nn $ setResultSuccess eid relay
          Right (Nostr.Response.OK eid False reason) -> do
            liftIO . flip runReaderT nn $ setResultError (fromMaybe "" reason) eid relay
          Right _ -> do 
               liftIO . logRelayError relay . pack $ "Uknown response: " <> show msg 
          Left errMsg -> do 
               liftIO . logRelayError relay . pack $ "Decoding failed with: " <> show errMsg <> " for response=" <> show msg

      WS.addEventListener socket "close" $ \e -> do
        code <- codeToCloseCode <$> WS.code e
        reason <- WS.reason e
        clean <- WS.wasClean e
        liftIO . sink . sendMsg $ (WebSocketClose relay $ decodeError code clean reason)
        liftIO . print $ "closed connection " <> show relay <> " because " <> show code <> show reason <> show clean
        reconnect
 
      WS.addEventListener socket "error" $ \v -> do
        d' <- WS.data' v
        undef <- ghcjsPure (isUndefined d')
        if undef
          then do
            liftIO . sink . sendMsg $ (WebSocketError relay mempty)
          else do
            Just d <- fromJSVal d'
            liftIO . sink . sendMsg $ (WebSocketError relay d)
        reconnect

      rc <- liftIO . atomically . dupTChan $ (nn ^. #requestCh)
      let doLoop =
            do
              state <- WS.socketState socket
              case state of
                0 -> do
                  -- not ready yet
                  liftIO . sleep . Seconds $ 0.1
                  doLoop
                1 -> do
                  -- ready
                  -- try reading requests to send
                  requests <- liftIO . collectJustM . atomically . tryReadTChan $ rc
                  mapM_ (sendJson' socket) requests
                  liftIO . sleep $ Seconds 0.05 -- TODO: 
                  doLoop
                2 -> markAllError relay "Relay closing connection"
                3 -> markAllError relay "Relay closed connection"
                _ -> markAllError relay "Error received from relay"
      doLoop

    markIsConnected isCon r =  
        modifyMVar_ (nn ^. #relays) $ \rels ->
          pure $ rels & at (r ^. #uri) % _Just % #connected .~ isCon

    markAllError relay eText = do 
      liftIO $ do 
        markIsConnected False relay 
        runNostr nn $ changeStateForAllSubs relay (fmap . const $ Nostr.Network.Error eText)

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