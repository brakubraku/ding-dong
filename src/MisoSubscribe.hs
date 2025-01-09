{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module MisoSubscribe where
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State (MonadState (get, put), StateT (runStateT))
import Data.Either
import Data.Text hiding (length)
import qualified Data.Text as T hiding (length)
import GHC.Generics (Generic)
import Miso hiding (at)
import Nostr.Filter
import Nostr.Log (logError)
import Nostr.Network
import Nostr.Relay
import Nostr.RelayPool as RP
import Nostr.Request (SubscriptionId)
import Nostr.Response
import Optics
import ModelAction (SubState (..))
import Data.Bool (bool)
import Utils
import Control.Monad (unless)
import qualified Data.Map as Map

-- 3 types of subscribes:
--   call actOnResults periodically with whatever messages you have received and quit after EOS
--   call actOnResults periodically with whatever messages you have received and never quit
--   call actOnResults only once after EOS, on all collected messages, and quit

-- Motivation here is that for example when receiving TextNotes and Deletes you want to be able to process
-- all of them at once, so that you don't display TextNotes which are Delete-d (in later messages).

data SubType = PeriodicUntilEOS | PeriodicForever | AllAtEOS
  deriving (Eq)

-- how often to poll for responses
period :: Seconds
period = Seconds 0.1

-- how long to wait for slower relays
defaultTimeout :: Seconds
defaultTimeout = Seconds 5

-- the ratio of EOSE/Running relays
-- this is to prevent the *AtEOS subscriptions from hanging
-- when some relays stop responding/are slow

acceptableRatio :: Float
acceptableRatio = 7 / 10

toMicro :: Seconds -> Int
toMicro s = round $ getSeconds s * fromInteger (10 ^ 6)

data SubData = SubData
  { msgsRecvd :: Int,
    msgs :: [(Response, Relay)],
    -- how much time longer to wait for relays to EOS,
    -- after acceptableRatio has been achieved
    timeout :: Seconds
  }
  deriving (Eq, Generic)

subscribe ::
  NostrNetwork ->
  SubType ->
  [DatedFilter] ->
  ([e] -> action) ->
  Maybe ((SubscriptionId, SubState) -> action) ->
  ((Response, Relay) -> Either Text e) ->
  Maybe (MVar ()) ->
  Sub action
subscribe nn subType subFilter actOnResults actOnSubState extractResults cancelButton sink = do
  (respChan, subId) <-
    liftIO . flip runReaderT nn $
      RP.subscribeForFilter subFilter

  let collectResponses = do
        subState <- liftIO $ Map.lookup subId <$> readMVar (nn ^. #subscriptions)
        relays <- liftIO $ Map.elems <$> readMVar (nn ^. #relays)
        rrs <-
          collectJustM . liftIO . atomically $
            tryReadTChan respChan
        let finished = isSubFinished subState 
        let ratio = ratioOfFinished subState
        -- inform about subscription state changes if
        -- function actOnSubState is provided 
        let reportSubState _ Nothing = pure ()
            reportSubState isFinished (Just act) = liftIO $ do 
              let relState = subState ^? _Just % #relaysState
                  state = (subId,) <$> bool SubRunning SubFinished isFinished <$> relState
              maybe
                ( logError $
                    "Could not find relays state for subId="
                      <> (T.pack . show $ subId)
                )
                (sink . act)
                state
        let reportRunning = reportSubState False actOnSubState 
        let reportFinished = reportSubState True actOnSubState
        let continue = do
              reportRunning 
              liftIO $ sleep period
              isCanceled <- maybe (pure False) isSubCanceled cancelButton
              unless isCanceled collectResponses
        case subType of
          AllAtEOS -> do
            addMessages rrs
            addStats (length rrs)
            sd@SubData {..} <- get
            case (finished, ratio >= acceptableRatio, getSeconds timeout <= 0) of
              (True, _, _) -> (liftIO . processMsgs $ msgs) >> reportFinished
              (_, True, True) -> (liftIO . processMsgs $ msgs) >> reportFinished
              (_, True, False) -> do
                put $ sd & #timeout %~ subtract period
                continue
              (_, _, _) -> continue
          PeriodicUntilEOS -> do
            addStats (length rrs)
            sd@SubData {..} <- get
            unless (length rrs == 0) $ 
              liftIO . processMsgs $ rrs
            case (finished, ratio >= acceptableRatio, getSeconds timeout <= 0) of
              (True, _, _) -> reportFinished
              (_, True, True) -> reportFinished
              (_, True, False) -> do
                put $ sd & #timeout %~ subtract period
                continue
              (_, _, _) -> continue
          PeriodicForever -> do
            addStats (length rrs)
            unless (length rrs == 0) $ 
             liftIO . processMsgs $ rrs  
            -- has any relay closed connection or is the subscription not running on all connected relays?
            let isRestartLongRunning = 
                 isAnyRelayError subState 
                  || isNotRunningOnAll subState relays
            unless isRestartLongRunning continue

  liftIO $ do 
     (_, SubData {..}) <- runStateT collectResponses (SubData 0 [] defaultTimeout)
     print $ "branko-Unsubscribing subId=" <> show subId <> " filter=" <> show subFilter <> "; msgs-received: " <> show msgsRecvd
     flip runReaderT nn $ RP.unsubscribe subId

  where
    processMsgs :: [(Response, Relay)] -> IO ()
    processMsgs rrs = do
      let processed = extractResults <$> rrs
      mapM_ logError $ lefts processed
      sink . actOnResults . rights $ processed

    addStats :: (Monad a) => Int -> StateT SubData a ()
    addStats n = do
      sd <- get
      put $ sd & #msgsRecvd %~ (+ n)

    addMessages :: (Monad a) => [(Response, Relay)] -> StateT SubData a ()
    addMessages ms = do
      sd <- get
      put $ sd & #msgs %~ (<> ms)

    -- don't want Num instance
    subtract (Seconds s1) (Seconds s2) = Seconds (s2-s1)

isSubCanceled :: MonadIO m => MVar () -> m Bool
isSubCanceled cancelButton = liftIO $ (maybe False (const True)) <$> tryReadMVar cancelButton

cancelSub :: MonadIO m => MVar () -> m ()
cancelSub cancelButton = liftIO $ putMVar cancelButton ()