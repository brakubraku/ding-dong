{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MisoSubscribe where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State (MonadState (get, put), StateT (StateT, runStateT))
import Data.Either
import qualified Data.Map as Map
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
import Control.Monad (unless)
import GHC.Float

-- 3 types of subscribes:
--   call actOnResults periodically with whatever messages you have received and quit after EOS
--   call actOnResults periodically with whatever messages you have received and never quit
--   call actOnResults only once after EOS, on all collected messages, and quit

-- Motivation here is that for example when receiving TextNotes and Deletes you want to be able to process
-- all of them at once, so that you don't display TextNotes which are Delete-d (in later messages).

data SubType = PeriodicUntilEOS | PeriodicForever | AllAtEOS
  deriving (Eq)

newtype Seconds = Seconds {
  getSeconds :: Float
} 
 deriving (Eq, Generic)
 deriving newtype (Num, Ord)

-- how often to poll for responses
period :: Seconds
period = Seconds 0.1 

-- the ratio of EOSE/Running relays
-- this is to prevent the *AtEOS subscriptions from hanging
-- when some relays stop responding/are slow

acceptableRatio :: Float
acceptableRatio = 7/10

toMicro :: Seconds -> Int 
toMicro (Seconds s) = float2Int $ s * fromInteger (10 ^ 6)

data SubData = SubData
  { msgsRecvd :: Int,
    msgs :: [(Response, Relay)],
    -- how much time longer to wait for relays to EOS,
    -- after acceptableRatio has been achieved
    timeout :: Seconds 
  }
  deriving (Eq, Generic)

-- TODO: add timeout to *AtEOS subscriptions
subscribe ::
  NostrNetwork ->
  SubType ->
  [DatedFilter] ->
  ([e] -> action) ->
  Maybe ((SubscriptionId, Map.Map Relay RelaySubState) -> action) ->
  ((Response, Relay) -> Either Text e) ->
  Sub action
subscribe nn subType subFilter actOnResults actOnSubState extractResults sink = do
  (respChan, subId) <-
    liftIO . flip runReaderT nn $
      RP.subscribeForFilter subFilter
  let collectResponses = do
        subStates <- liftIO . readMVar $ (nn ^. #subscriptions)
        liftIO $ reportSubState actOnSubState subId subStates
        rrs <-
          collectJustM . liftIO . atomically $
            tryReadTChan respChan
        let finished = isSubFinished subId subStates
        let ratio = ratioOfFinished subId subStates 
        let continueCollecting = do
              liftIO . threadDelay . toMicro $ period
              collectResponses
        case subType of
          AllAtEOS -> do
            addMessages rrs
            addStats (length rrs)
            sd@SubData {..} <- get
            case (finished, ratio >= acceptableRatio, getSeconds timeout > 0) of 
              (True,_,_) -> do
                liftIO . processMsgs $ msgs
                pure ()
              (False, True, False) -> do
                put $ sd & #timeout %~ (-) period
                liftIO . processMsgs $ msgs
                pure ()
              (_,_,_) -> continueCollecting
          PeriodicUntilEOS -> do
            addStats (length rrs)
            liftIO . processMsgs $ rrs
            unless finished continueCollecting
          PeriodicForever -> do
            addStats (length rrs)
            liftIO . processMsgs $ rrs
            continueCollecting

  liftIO $ do
    (_, SubData {..}) <- runStateT collectResponses (SubData 0 [] (Seconds 2))
    print $ "branko-Unsubscribing " <> show subFilter <> "; msgs-received: " <> show msgsRecvd
    flip runReaderT nn $ RP.unsubscribe subId
  where
    collectJustM :: (MonadIO m) => m (Maybe a) -> m [a]
    collectJustM action = do
      x <- action
      case x of
        Nothing -> pure []
        Just x -> do
          xs <- collectJustM action
          return (x : xs)

    processMsgs :: [(Response, Relay)] -> IO ()
    processMsgs rrs = do
      let processed = extractResults <$> rrs
      mapM_ logError $ lefts processed
      sink . actOnResults . rights $ processed

    -- inform about subscription state changes if
    -- function actOnSubState is provided
    reportSubState Nothing _ _ = pure ()
    reportSubState (Just act) subId subStates = do
      let state = (subId,) <$> subStates ^? at subId % _Just % #relaysState
      maybe
        ( logError $
            "Could not find relays state for subId="
              <> (T.pack . show $ subId)
        )
        (sink . act)
        state

    addStats :: Monad a => Int -> StateT SubData a ()
    addStats n = do
      sd <- get
      put $ sd & #msgsRecvd %~ (+ n)   
      
    addMessages :: Monad a => [(Response, Relay)] -> StateT SubData a ()
    addMessages ms = do
      sd <- get
      put $ sd & #msgs %~ (<> ms)