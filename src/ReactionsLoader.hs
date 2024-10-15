{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ReactionsLoader where

import Control.Concurrent
import Data.Text
import Nostr.Event
import Nostr.Filter
import Nostr.Reaction
import Nostr.Relay
import Nostr.Response
import PeriodicLoader
import qualified Data.Set as S
import Utils

createReactionsLoader :: IO (PeriodicLoader EventId (ReactionEvent, Relay))
createReactionsLoader = do
  buffers <- newMVar $ LoaderData S.empty S.empty
  let createFilter = \eids -> [DatedFilter (ReactionsTo eids) Nothing Nothing] -- TODO: Nothing Nothing
      extract = extractReactEvent
      period = Seconds 0.2 
  pure $ PeriodicLoader {..}

extractReactEvent :: (Response, Relay) -> Either Text (ReactionEvent, Relay)
extractReactEvent (resp, rel) = do
  event <- getEventOrError resp
  re <- maybe (Left "Event is not a reaction!") Right $ Nostr.Reaction.extract event
  pure (re, rel)
