{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
-- optics support
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Nostr.Network where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Reader
import Data.List (partition)
import Data.Map hiding (partition)
import qualified Data.Map as Map hiding (partition)
import qualified Data.Set as Set
import Data.Maybe
import Data.Text hiding (partition, zip)
import GHC.Float
import GHC.Generics
import Nostr.Keys
import Nostr.Relay
import Nostr.Request
import Nostr.Response hiding (EOSE)
import Optics
import Nostr.Event
import Data.Time

data SubscriptionState = SubscriptionState
  { relaysState :: Map Relay RelaySubState,
    responseCh :: TChan (Response, Relay)
  }
  deriving (Generic, Eq)

data RequestResult = ResultUnknown | ResultSuccess | ResultError Text
  deriving Eq

printState :: SubscriptionState -> Text
printState ss =
  let printRel (r, s) = r ^. #uri <> ":" <> pack (show s)
   in intercalate "\n" $ printRel <$> Map.toList (ss ^. #relaysState)

data NostrNetwork = NostrNetwork
  { relays :: MVar (Map.Map RelayURI Relay),
    subscriptions :: MVar (Map SubscriptionId SubscriptionState),
    requestResults :: MVar (Map EventId ((Map Relay RequestResult), UTCTime)),
    requestCh :: TChan Request,
    keys :: Keys
  }
  deriving (Generic, Eq)

runNostr :: NostrNetwork -> NostrNetworkT a -> IO a
runNostr = flip runReaderT

type NostrNetworkT = ReaderT NostrNetwork IO

data RelaySubState = Running | EOSE | Error Text deriving (Eq, Show)

-- Subscription is considered finished when none of Relays is in a Running state
-- for that particular subscription Id.
isSubFinished :: Maybe SubscriptionState -> Bool
isSubFinished mss =
  fromMaybe True $ do
    subState <- mss
    pure . notElem Running . elems $ subState ^. #relaysState

isAnyRelayError :: Maybe SubscriptionState -> Bool
isAnyRelayError mss =
  fromMaybe False $ do
    subState <- mss
    pure . Prelude.any isError . elems $ subState ^. #relaysState
  where 
    isError (Error _) = True
    isError _ = False

-- check if a subscription is being "run" on all connected relays
isNotRunningOnAll :: Maybe SubscriptionState -> [Relay] -> Bool
isNotRunningOnAll mss rels = 
  let connectedRels = rels ^.. folded % filtered connected % #uri
      subRelays = fromMaybe [] $ do
         subState <- mss
         pure . Map.keys $ subState ^. #relaysState
  in 
    Set.fromList connectedRels /= Set.fromList (subRelays ^.. folded % #uri)

ratioOfFinished :: Maybe SubscriptionState -> Float
ratioOfFinished mss = fromMaybe 1 $ do
  rs <- Map.elems . view #relaysState <$> mss
  let (running, other) = partition (== Running) rs
      (eose, _) = partition (== EOSE) other
      length = toInteger . Prelude.length
      ratio =
        int2Float (fromInteger . length $ eose)
          / int2Float (fromInteger . length $ running ++ eose) -- TODO: wtf
  pure ratio

initNetwork :: [Relay] -> Keys -> IO NostrNetwork
initNetwork rels keys = do
  relays <- newMVar . fromList . zip (view #uri <$> rels) 
    $ rels & traversed % #connected .~ False
  requestResults <- newMVar Map.empty
  requestCh <- atomically newTChan
  subscriptions <- newMVar Map.empty
  pure NostrNetwork {..}

newRelay :: RelayURI -> Relay
newRelay ru =
  Relay
    { uri = ru,
      connected = False,
      info = RelayInfo True True
    }

getResults :: EventId -> NostrNetworkT (Maybe (Map Relay RequestResult, UTCTime))
getResults eid = do
  nn <- ask
  reqs <- liftIO $ readMVar $ nn ^. #requestResults
  pure $ reqs ^. at eid

checkResult :: Map Relay RequestResult -> Float
checkResult results = do
      let r = Map.toList results
      let (ok, other) = partition ((== ResultSuccess) . snd) r
      let (unknown, error) = partition ((== ResultUnknown) . snd) other
      let length = toInteger . Prelude.length
          ratio =
            int2Float (fromInteger . length $ ok)
              / int2Float (fromInteger . length $ unknown)
      ratio

setResultSuccess :: EventId -> Relay -> NostrNetworkT ()
setResultSuccess = setResult ResultSuccess

setResultError :: Text -> EventId -> Relay -> NostrNetworkT ()
setResultError er = setResult (ResultError er)

setResult :: RequestResult -> EventId -> Relay -> NostrNetworkT ()
setResult res eid r = do 
  nn <- ask
  liftIO $ modifyMVar_ (nn ^. #requestResults) $ 
    \rr -> do 
      pure $ rr & at eid % _Just % _1 % at r ?~ res