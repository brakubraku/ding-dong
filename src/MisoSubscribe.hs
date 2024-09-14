{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module MisoSubscribe where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Either
import qualified Data.Map as Map
import Data.Maybe
import Data.Text hiding (length)
import qualified Data.Text as T hiding (length)
import Miso hiding (at)
import Nostr.Filter
import Nostr.Log (logError)
import Nostr.Network
import Nostr.Relay
import Nostr.RelayPool as RP
import Nostr.Request (SubscriptionId)
import Nostr.Response
import Optics

-- 3 types of subscribes:
--   call actOnResults periodically with whatever messages you have received and quit after EOS
--   call actOnResults periodically with whatever messages you have received and never quit
--   call actOnResults only once after EOS, on all collected messages, and quit

-- Motivation here is that for example when receiving TextNotes and Deletes you want to be able to process
-- all of them at once, so that you don't display TextNotes which are Delete-d (in later messages).

data SubType = PeriodicUntilEOS | PeriodicForever | AllAtEOS
  deriving (Eq)

data Stats = Stats
  { msgsRecvd :: Int
  }

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

  let collectResponses collectedSoFar stats = do
        subStates <- readMVar (nn ^. #subscriptions)
        reportSubState actOnSubState subId subStates
        rrs <-
          collectJustM . liftIO . atomically $
            tryReadTChan respChan

        -- rrs <-
        --   catMaybes
        --     . Prelude.takeWhile (isJust)
        --     <$> ( sequence
        --             . repeat
        --             . liftIO
        --             . atomically
        --             . tryReadTChan
        --             $ respChan
        --         )

        let finished = isSubFinished subId $ subStates
        let continueCollecting csf stats = do
              threadDelay $ 10 ^ 5 -- TODO
              collectResponses csf stats
        case subType of
          AllAtEOS ->
            if finished
              then do
                processMsgs $ collectedSoFar ++ rrs
                pure $ addStats (length rrs) stats
              else do
                continueCollecting (collectedSoFar ++ rrs) $ addStats (length rrs) stats
          PeriodicUntilEOS -> do
            processMsgs rrs
            if finished
              then
                pure $ addStats (length rrs) stats
              else
                continueCollecting [] $ addStats (length rrs) stats
          PeriodicForever -> do
            processMsgs rrs
            continueCollecting [] stats

  liftIO $ do
    Stats howMany <- collectResponses [] (Stats 0)
    print $ "branko-Unsubscribing " <> show subFilter <> "; msgs-received: " <> show howMany
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

    addStats :: Int -> Stats -> Stats
    addStats n (Stats howMany) = Stats $ howMany + n