{-# LANGUAGE OverloadedStrings   #-}

module Nostr.Request where

import Data.Aeson
import Data.DateTime
import Data.Text                    (Text, pack)
import GHC.Exts                     (fromList)

import Nostr.Event
import Nostr.Filter
import Nostr.Relay

type SubscriptionId = Text

data Subscription = Subscription
  { filters :: [DatedFilter]
  , subId   :: SubscriptionId
  }
  deriving (Eq, Show)

data Request
  = SendEvent Event
  | Subscribe Subscription
  | Close SubscriptionId
  | Disconnect Relay
  deriving (Eq, Show)

instance ToJSON Request where
  toJSON sr = case sr of
    SendEvent e -> Array $ fromList
       [ String "EVENT"
       , toJSON e
       ]
    Subscribe (Subscription efs s) -> Array $ fromList
      ([ String "REQ"
      , String s
       ] ++ map toJSON efs)
    Close subId -> Array $ fromList
       [ String "CLOSE"
       , String subId
       ]
    Disconnect r -> String "Bye!"
