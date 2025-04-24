{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module PeriodicLoader where

import Control.Concurrent
import Control.Monad (when, void)
import Control.Monad.IO.Class
import Data.Set as Set (Set, difference, fromList, map, null, toList, union, empty)
import GHC.Generics
import MisoSubscribe (subscribe, SubType (PeriodicUntilEOS), SubscriptionParams (..))
import Nostr.Filter
import Nostr.Network
import Nostr.Relay
import Nostr.Response
import Optics
import Miso (forkJSM, JSM, Sub)
import Data.Text
import Debug.Trace
import Utils

data LoaderData id = LoaderData
  { loading :: Set id,
    loaded :: Set id
  }
  deriving (Generic)

data PeriodicLoader id e = PeriodicLoader
  { buffers :: MVar (LoaderData id),
    createFilter :: [id] -> [DatedFilter],
    extract :: (Response, Relay) -> Either Text e,
    period :: Seconds 
  }
  deriving (Generic)

load :: (Ord id) => PeriodicLoader id e -> [id] -> JSM ()
load pl ids = do
  liftIO . modifyMVar_ (pl ^. #buffers) $ \b -> do
    pure $ b & #loading %~ union (Set.fromList ids)

startLoader ::
  (Ord id, Show id) =>
  NostrNetwork ->
  PeriodicLoader id e ->
  ([e] -> action) ->
  (Text -> action) ->
  Sub action
startLoader nn pl actOnResults actOnError sink =
  let loop = do
        toLoad <- liftIO $ modifyMVar (pl ^. #buffers) $ \b -> do
          let toLoad =
                (b ^. #loading)
                  `difference` (b ^. #loaded)
          pure $
            ( b & #loaded %~ Set.union toLoad
                & #loading .~ Set.empty, 
                toLoad
            )
        -- traceM $ "toLoad=" <> (show $ toLoad)
        when (not . Set.null $ toLoad) $ do
          void . forkJSM . subscribe nn sink $ 
              SubscriptionParams
                { subType = PeriodicUntilEOS,
                  subFilter = ((pl ^. #createFilter) . toList $ toLoad) ,
                  extractResults = pl ^. #extract,
                  actOnResults = actOnResults,
                  actOnSubState = Nothing,
                  cancelButton = Nothing,
                  timeoutPerRelay = Nothing,
                  reportError = actOnError
                }
        liftIO . sleep $ pl ^. #period
        loop
  in traceM "starting loader" >> loop
