{-# LANGUAGE OverloadedStrings #-}

module Contacts where

import Control.Monad.IO.Class
import Data.Time
import MyCrypto
import Nostr.Event
import Nostr.Filter
import Nostr.Keys
import Nostr.Network
import Nostr.Profile
import Nostr.RelayPool
import Nostr.Request
import Nostr.Log

saveContacts :: Keys -> [(XOnlyPubKey, Maybe Username)] -> NostrNetworkT ()
saveContacts (Keys kp xo _ _) contacts = do
  now <- liftIO getCurrentTime
  let unsigned = setContacts contacts xo now
  case signEvent unsigned kp xo of 
    Just signed -> send . SendEvent $ signed
    Nothing -> liftIO . logError $ "Failed signing event!"