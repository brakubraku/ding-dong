{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module PeriodicLoader where

import Control.Concurrent
import Control.Monad (unless)
import Control.Monad.IO.Class
import Data.Set as Set (Set, fromList, toList, union, empty)
import GHC.Generics
import MisoSubscribe (subscribe, SubType (PeriodicUntilEOS), SubscriptionParams (..))
import Language.Javascript.JSaddle
import Nostr.Filter
import Nostr.Network
import Nostr.Relay
import Nostr.Response
import Optics
import Miso (Sub, Sink)
import Miso.String hiding (null, zip)
import Debug.Trace
import Utils
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import qualified Data.Map as M
import Data.List ((\\))

data LoaderData id = LoaderData
  { loading :: Set id,
    loaded :: M.Map id UTCTime
  }
  deriving (Generic)

data PeriodicLoader id e = PeriodicLoader
  { buffers :: MVar (LoaderData id),
    createFilter :: [id] -> [DatedFilter],
    extract :: (Response, Relay) -> Either MisoString e,
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
  (MisoString -> action) ->
  Sub action
startLoader nn pl actOnResults actOnError sink =
  let loop = do
        toLoad <- liftIO $ modifyMVar (pl ^. #buffers) $ \b -> do
          now <- getCurrentTime
          -- reload elements older than 10 minutes
          let loadedFresh = M.filter (youngerThan now (Minutes 10)) (b ^. #loaded)
          let toLoad =
                toList (b ^. #loading) \\ (M.keys loadedFresh)
          pure $
            ( b & #loaded .~ (M.union loadedFresh . M.fromList . zip toLoad $ repeat now)
                & #loading .~ Set.empty, 
                toLoad
            )
        unless (null toLoad) $ do
          startSubscription nn sink $
            SubscriptionParams
              { subType = PeriodicUntilEOS,
                subFilter = pl ^. #createFilter $ toLoad,
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

startSubscription :: NostrNetwork -> Sink action -> SubscriptionParams action -> JSM ()
startSubscription nn sink sp = subscribe nn sp sink
--  where
  -- subName = "PeriodicLoader" <> (show . hash) (show sp)

forkJSM :: JSM () -> JSM ThreadId
forkJSM a = do
  ctx <- askJSM
  liftIO (forkIO (runJSM a ctx))

newtype Minutes = Minutes Integer

youngerThan :: UTCTime -> Minutes -> UTCTime -> Bool 
youngerThan now (Minutes mins) time = abs (diffUTCTime now time) < fromInteger (mins * 60)

