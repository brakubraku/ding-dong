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

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Reader
import           Data.Aeson
import           Data.ByteString (fromStrict)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Text.Encoding (encodeUtf8)
import           Data.Text
import           GHCJS.Foreign
import           GHCJS.Marshal
import           GHCJS.Types ()
import           Language.Javascript.JSaddle.String
import           Language.Javascript.JSaddle.Value (valToStr)
import           Miso hiding (WebSocket(..), at)
import           Language.Javascript.JSaddle
import           Miso.String (ms, MisoString)
import           Nostr.Log
import           Nostr.Network
import           Nostr.Relay
import           Nostr.RelayPool
import           Nostr.Response
import           Nostr.Request
import           Optics hiding ((#))
import           Prelude hiding (map)
import           Data.Time
import           Nostr.Event
import           Utils
import Debug.Trace

data WebSocketAction =
  WebSocketOpen Relay |
  WebSocketClose Relay MisoString |
  WebSocketError Relay MisoString

connectRelays ::
  NostrNetwork ->
  (WebSocketAction -> action) ->
  Sub action
connectRelays nn sendMsg sink = do
  -- connect a relay
  relays <- liftIO $ readMVar (nn ^. #relays)
  now <- liftIO getCurrentTime
  mapM_ (forkJSM . conRelay (0, now) Nothing) relays
  where
    conRelay :: (Int, UTCTime) -> Maybe (TChan Request)-> Relay -> JSM ()
    conRelay (recnt, lastReconnect) mReqCh relay = do
      isReconnectingMVar <- liftIO $ newMVar False
      socket <- createWebSocket (ms (relay ^. #uri)) []
      rc <- case mReqCh of 
        Nothing -> liftIO . atomically . dupTChan $ (nn ^. #requestCh)
        Just ch -> pure ch
      let
        reconnect reqCh =
          do
            isReconnecting <- liftIO $ readMVar isReconnectingMVar
            unless isReconnecting $ do
              liftIO $ markIsConnected False relay
              liftIO $ modifyMVar_ isReconnectingMVar (const . pure $ True)
              now <- liftIO getCurrentTime
              liftIO . print $ show now <> ": reconnecting " <> show relay
              let diff = (round $ diffUTCTime now lastReconnect)
              case (recnt > 3, diff > 1) of -- TODO: take time into account?
                (True, _) -> do
                  liftIO . sleep . Seconds $ 5
                  conRelay (0, now) (Just reqCh) relay
                (False, _) -> do
                  liftIO . sleep . Seconds $ 0.5
                  conRelay (recnt + 1, now) (Just reqCh) relay

      addEventListener (getSocket socket) "open" $ \_ -> do
         do
          liftIO $ markIsConnected True relay
          sink . sendMsg $ WebSocketOpen relay

      addEventListener (getSocket socket) "message" $ \v -> do
        msg <- valToStr =<< v ! ("data" :: MisoString)
        let msgToParse = fromStrict . encodeUtf8 . strToText $ msg
        resp <-
          pure . eitherDecode @Response $ msgToParse
        hashableResp <-
          pure . eitherDecode @HashableResponse $ msgToParse
        case (resp, hashableResp) of
          (Right (EventReceived subId event), Right (HashableEventReceived _ he))-> do
            subs <- liftIO . readMVar $ (nn ^. #subscriptions)
            case verifySignature (event, he) of 
              False -> 
                liftIO . logRelayError relay . pack
                  $ "Failed signature verification of event="  
                    <> show event <> " from msg=" <> show msg
              True ->
                case Map.lookup subId subs of
                  Just subscription -> do
                    liftIO $
                      atomically $
                        writeTChan
                          (subscription ^. #responseCh)
                          (EventReceived subId event, relay)
                  Nothing -> do
                    liftIO $ logRelayError relay
                      . pack
                      $ "SubId="
                        <> show subId
                        <> " not found in responseChannels. Event received="
                        <> show event

          (Right (Nostr.Response.EOSE subId), _) -> do
            liftIO . runReaderT (changeState subId relay (fmap . const $ Nostr.Network.EOSE)) $ nn
          (Right (Nostr.Response.OK eid True _), _) -> do
            liftIO . flip runReaderT nn $ setResultSuccess eid relay
          (Right (Nostr.Response.OK eid False reason), _) -> do
            liftIO . flip runReaderT nn $ setResultError (fromMaybe "" reason) eid relay
          (Right _, _) -> do
               liftIO $ logRelayError relay . pack $ "Uknown response: " <> show msg
          (Left errMsg, _) -> do
               liftIO $ logRelayError relay . pack $
                "Decoding failed with: " <> show errMsg <> " for response=" <> show msgToParse

      addEventListener (getSocket socket) "close" $ \e -> do
        code <- codeToCloseCode <$> getCode e
        reason <- getReason e
        clean <- wasClean e
        sink . sendMsg $ (WebSocketClose relay $ decodeError code clean reason)
        liftIO . print $ "closed connection " <> show relay <> " because " <> show code <> show reason <> show clean
        reconnect rc

      addEventListener (getSocket socket) "error" $ \v -> do
        d' <- v ! ("data" :: MisoString)
        undef <- ghcjsPure (isUndefined d')
        if undef
          then do
            sink . sendMsg $ (WebSocketError relay mempty)
          else do
            Just d <- fromJSVal d'
            sink . sendMsg $ (WebSocketError relay d)
        reconnect rc

      let doLoop =
            do
              state <- socketState socket
              case state of
                0 -> do
                  -- not ready yet
                  liftIO . sleep . Seconds $ 0.1
                  doLoop
                1 -> do
                  -- ready
                  -- try reading requests to send
                  requests <- liftIO . collectJustM . atomically . tryReadTChan $ rc
                  forM_ requests $ \r -> traceM ("branko-request: " <> show r)
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
sendJson' socket m = sendSocket socket =<< jsonStringify m

createWebSocket :: MisoString -> [MisoString] -> JSM Socket
{-# INLINE createWebSocket #-}
createWebSocket url' protocols = createSocket url' =<< toJSVal protocols

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

-----------------------------------------------------------------------------
newtype Socket = Socket { getSocket :: JSVal }
-----------------------------------------------------------------------------
createSocket :: MisoString -> JSVal -> JSM Socket
createSocket url protocols = Socket <$> new (jsg ("WebSocket" :: JSString)) (url, protocols)
-----------------------------------------------------------------------------
socketState :: Socket -> JSM Int
socketState (Socket s) = fromJSValUnchecked =<< s ! ("readyState" :: JSString)
-----------------------------------------------------------------------------
wasClean :: JSVal -> JSM WasClean
wasClean v = WasClean <$> (fromJSValUnchecked =<< v ! ("wasClean" :: JSString))
-----------------------------------------------------------------------------
getCode :: JSVal -> JSM Int
getCode v = fromJSValUnchecked =<< v ! ("code" :: JSString)
-----------------------------------------------------------------------------
getReason :: JSVal -> JSM Reason
getReason v = Reason <$> (fromJSValUnchecked =<< v ! ("reason" :: JSString))
-----------------------------------------------------------------------------
closeSocket :: Socket -> JSM ()
closeSocket (Socket s) = do
  _ <- s # ("close" :: JSString) $ ([] :: [JSString])
  pure ()
-----------------------------------------------------------------------------
sendSocket :: Socket -> MisoString -> JSM ()
sendSocket (Socket s) msg = do
  _ <- s # ("send" :: JSString) $ [msg]
  pure ()
-----------------------------------------------------------------------------
forkJSM :: JSM () -> JSM ThreadId
forkJSM a = do
  ctx <- askJSM
  liftIO (forkIO (runJSM a ctx))
