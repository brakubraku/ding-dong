{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module ProfilesLoader (createProfilesLoader) where

import PeriodicLoader

import Control.Concurrent
import Data.Text
import Nostr.Event
import Nostr.Filter
import Nostr.Relay
import Nostr.Response
import MyCrypto
import Nostr.Profile
import Data.DateTime
import qualified Data.Set as S

createProfilesLoader :: IO (PeriodicLoader XOnlyPubKey (XOnlyPubKey, Profile, DateTime, Relay))
createProfilesLoader = do
  buffers <- newMVar $ LoaderData S.empty S.empty
  let createFilter = \xos -> [DatedFilter (MetadataFilter xos) Nothing Nothing] -- TODO: Nothing Nothing
      extract = extractProfile
      period = 300 -- every 300 miliseconds
  pure $ PeriodicLoader {..}

extractProfile :: (Response, Relay) -> Either Text (XOnlyPubKey, Profile, DateTime, Relay)
extractProfile (resp, rel) = do
  event <- getEventOrError resp
  re <- maybe (Left "Event is not a profile!") Right $ extractProfileFromResp (event, rel)
  pure re

extractProfileFromResp ::
  (Event, Relay) ->
  Maybe (XOnlyPubKey, Profile, DateTime, Relay)
extractProfileFromResp (event, relay) = parseProfiles event
  where
    parseProfiles e =
      let xo = pubKey e
       in case readProfile e of
            Just p -> Just (xo, p, created_at e, relay)
            Nothing -> Nothing