{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module MisoSubscribe where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Either
import qualified Data.Map as Map
import Data.Maybe
import Data.Text
import qualified Data.Text as T
import Miso hiding (at)
import Nostr.Filter
import Nostr.Log (logError)
import Nostr.Network
import Nostr.Relay
import Nostr.RelayPool as RP
import Nostr.Request (SubscriptionId)
import Nostr.Response
import Optics

subscribe ::
  NostrNetwork ->
  [DatedFilter] ->
  ([e] -> action) ->
  Maybe ((SubscriptionId, Map.Map Relay RelaySubState) -> action) ->
  ((Response, Relay) -> Either Text e) ->
  Sub action
subscribe nn subFilter actOnResults actOnSubState extractResults sink = do
  (respChan, subId) <-
    liftIO . flip runReaderT nn $
      RP.subscribeForFilter subFilter
  let collectResponses = do
        subStates <- readMVar (nn ^. #subscriptions)
        -- putStrLn $ -- TODO: send this to hell
          -- "1. substates are:"
            -- <> show (relaysState <$> Map.elems subStates)

        -- inform about subscription state changes if 
        -- function actOnSubState is provided
        let actOnStateChange = fromMaybe (pure ()) $ do
              action <- actOnSubState
              let state = do
                    relState <- subStates ^? at subId % _Just % #relaysState
                    pure (subId, relState)

              Just $
                maybe
                  ( logError $
                      "Could not find relays state for subId="
                        <> (T.pack . show $ subId)
                  )
                  (sink . action)
                  state
        actOnStateChange

        let finished = isSubFinished subId $ subStates
        msgs <-
          collectJustM . liftIO . atomically $
            tryReadTChan respChan
        let processed = extractResults <$> msgs
        mapM_ logError $ lefts processed
        sink . actOnResults . rights $ processed
        threadDelay $ 10 ^ 5 -- TODO
        unless finished collectResponses
        -- putStrLn $ -- TODO: send this to hell
          -- "branko:Subscription finished " <> show subFilter
  liftIO collectResponses
  where
    collectJustM :: (MonadIO m) => m (Maybe a) -> m [a]
    collectJustM action = do
      x <- action
      case x of
        Nothing -> return []
        Just x -> do
          xs <- collectJustM action
          return (x : xs)
