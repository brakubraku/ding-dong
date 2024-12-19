{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module ProfilesLoader where

import PeriodicLoader

import Control.Concurrent
import Data.Text
import Nostr.Event
import Nostr.Filter
import Nostr.Relay
import Nostr.Response
import MyCrypto
import qualified Data.Set as S
import Utils
import Optics
import Data.Maybe (catMaybes)
import Data.Bifunctor
import Nostr.Kind (Kind(Metadata, RelayList))
import ProfilesLoader.Types
import Data.DateTime (fromSeconds)

createProfilesLoader :: IO (PeriodicLoader XOnlyPubKey ProfOrRelays)
createProfilesLoader = do
  buffers <- newMVar $ LoaderData S.empty S.empty
  let createFilter = \xos -> [DatedFilter (MetadataFilter xos) Nothing Nothing, 
                              DatedFilter (RelayListMetadata xos) Nothing Nothing] -- TODO: Nothing Nothing
      extract = extractProfile
      period = Seconds 0.2 
  pure $ PeriodicLoader {..}

extractProfile :: (Response, Relay) -> Either Text ProfOrRelays
extractProfile (resp, rel) = do
  event <- getEventOrError resp
  let profOrRels = extractProfileFromResp (event, rel)
  result <- case profOrRels of 
    (_, Nothing, Nothing) -> Left "Event is not a profile!"
    _ -> Right profOrRels
  pure result

extractProfileFromResp ::
  (Event, Relay) ->
  ProfOrRelays
extractProfileFromResp (event, relay) 
  | event ^. #kind == Metadata = (xo, parseProfiles event, Nothing)
  | event ^. #kind == RelayList = (xo, Nothing, parseRelays event)
  | otherwise = (xo, Nothing, Nothing)
  where
    xo = pubKey event
    parseProfiles e =
          case readProfile e of
            Just p -> Just (p, fromSeconds $ created_at e, relay)
            Nothing -> Nothing
    parseRelays e = 
         let rtags = first (catMaybes . fmap rTagToRelay) (getRTags e, fromSeconds $ e ^. #created_at)
         in case fst rtags of 
              [] -> Nothing
              _ -> Just rtags