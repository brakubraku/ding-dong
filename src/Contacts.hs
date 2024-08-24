{-# LANGUAGE OverloadedStrings #-}

module Contacts where

import Control.Monad.IO.Class
import Data.Time
import MyCrypto
import Nostr.Event
import Nostr.Keys
import Nostr.Network
import Nostr.Profile
import Nostr.RelayPool
import Nostr.Request
import Nostr.Log

saveContacts :: Keys -> [(XOnlyPubKey, Maybe Username)] -> NostrNetworkT ()
saveContacts (Keys sk xo _) contacts = do
  now <- liftIO getCurrentTime
  let unsigned = setContacts contacts xo now
  case signEvent unsigned sk xo of 
    Just signed -> send . SendEvent $ signed
    Nothing -> liftIO . logError $ "Failed signing event!"