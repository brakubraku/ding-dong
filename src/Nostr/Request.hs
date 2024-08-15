{-# LANGUAGE OverloadedStrings   #-}

module Nostr.Request where

-- import Control.Concurrent.STM.TChan
-- import Control.Monad.STM            (atomically)
-- import Crypto.Random.DRBG           (CtrDRBG, genBytes, newGen, newGenIO)
import Data.Aeson
-- import Data.DateTime
import Data.Text                    (Text, pack)
import GHC.Exts                     (fromList)

-- import qualified Data.ByteString.Base16 as B16

-- import Nostr.Event
import Nostr.Filter
-- import Nostr.Relay

type SubscriptionId = Text

data Subscription = Subscription
  { filters :: [Filter]
  , subId   :: SubscriptionId
  }
  deriving (Eq, Show)

data Request
  = Subscribe Subscription
  deriving (Eq, Show)

instance ToJSON Request where
  toJSON (Subscribe (Subscription efs s)) = 
    Array $ fromList ([ String "REQ", String s] ++ map toJSON efs)
    