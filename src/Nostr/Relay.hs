{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE InstanceSigs #-}

module Nostr.Relay where

import Data.Aeson
import Data.Text (Text)
import GHC.Exts (fromList)
import GHC.Generics

data RelayInfo = RelayInfo
  { readable  :: Bool
  , writable  :: Bool
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

type RelayURI = Text
data Relay = Relay
  { 
    uri :: RelayURI
  , info      :: RelayInfo
  , connected :: Bool
  }
  deriving (Show, Generic)

instance Eq Relay where 
  (==) = \r1 r2 -> uri r1 == uri r2

instance Ord Relay where
  compare (Relay r _ _) (Relay r' _ _) = compare r r'

instance FromJSON Relay where
  parseJSON = withObject "Relay" $ \r -> do
    uri'  <- r .: "uri"
    info' <- r .: "info"
    return $ Relay uri' info' False

instance ToJSON Relay where
  toJSON r = object $ fromList
    [ ( "uri", String $ uri r)
    , ( "info", toJSON $ info r)
    ]
  