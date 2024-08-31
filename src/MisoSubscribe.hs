{-# LANGUAGE OverloadedLabels #-}

module MisoSubscribe where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Data.Map as Map
import Miso
import Nostr.Filter
import Nostr.Network
import Nostr.Relay
import Nostr.RelayPool as RP
import Nostr.Response
import Optics
import Data.Maybe
import Data.Either
import Nostr.Log (logError)
import Data.Text

subscribe ::
  NostrNetwork ->
  [DatedFilter] ->
  ([e] -> action) -> 
  ((Response, Relay) -> Either Text e) ->
  Sub action
subscribe nn subFilter act process sink = do
  (respChan, subId) <-
    liftIO . flip runReaderT nn $
      RP.subscribeForFilter subFilter
  let collectResponses = do
        subsState <- readMVar (nn ^. #subscriptions)
        putStrLn $ -- TODO: send this to hell
          "1. substates are:"
            <> show (relaysState <$> Map.elems subsState)
        let finished = isSubFinished subId $ subsState
        msgs <-
          collectJustM . liftIO . atomically $
            tryReadTChan respChan
        let processed = process <$> msgs
        mapM_ logError $ lefts processed
        sink . act . rights $ processed
        threadDelay $ 10^6 -- TODO
        unless finished collectResponses
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

