{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Nostr.NewRequest where

import Data.Aeson
import Data.Text                    (Text)

import Nostr.Event
import qualified Data.Vector as V
import MyCrypto (XOnlyPubKey)
import Nostr.Kind
import Data.Time
import Data.Aeson.Types (parseMaybe)

type SubscriptionId = Text

data Subscription = Subscription
  { filters :: [Filter]
  , subId   :: SubscriptionId
  }
  deriving (Eq, Show)

data NewRequest
  = SendEvent Event
  | Subscribe Subscription
  | Close SubscriptionId
  deriving (Eq, Show)

instance FromJSON NewRequest where 
  parseJSON = withArray "Nostr request" $ \arr -> do
    type' <- parseJSON $ arr V.! 0
    case type' of
      String "EVENT" -> do
        e <- parseJSON $ arr V.! 2
        pure $ SendEvent e
      String "CLOSE" -> do
        subId <- parseJSON $ arr V.! 1
        pure $ Close subId
      String "REQ" -> do 
        subId <- parseJSON $ arr V.! 1
        filters <- parseFilters $ V.drop 2 arr
        pure . Subscribe $ Subscription (V.toList filters) subId
      _ -> fail "Unknown Nostr request"
   where 
    parseFilters = mapM parseJSON

data Filter = Filter {
  ids :: Maybe [EventId],
  authors :: Maybe [XOnlyPubKey],
  kinds :: [Kind],
  tags :: Maybe [Tag],
  since :: Maybe Int,
  until :: Maybe Int,
  limit :: Maybe Int
}
 deriving (Eq, Show)

instance FromJSON Filter where 
  parseJSON = withObject "Filter" $ \f -> 
    Filter
     <$> f .:? "ids" 
     <*> f .:? "authors" 
     <*> f .: "kinds" 
     <*> f .:? "tags"
     <*> f .:? "since" 
     <*> f .:? "until" 
     <*> f .:? "limit" 

-- ddf = decode "{\"since\":1234,\"kinds\":[0]}" :: Maybe Filter
