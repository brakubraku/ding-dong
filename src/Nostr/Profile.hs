{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE OverloadedLabels   #-}
{-# LANGUAGE DeriveGeneric #-}

module Nostr.Profile where

import Data.Aeson
import Data.Default
import GHC.Generics
import Miso.String (MisoString)

type RelayURL = MisoString

type Username = MisoString

type DisplayName = MisoString

type About = MisoString

type Picture = MisoString

data Profile = Profile {
  username :: MisoString ,
  displayName :: Maybe DisplayName,
  about :: Maybe About,
  picture :: Maybe Picture,
  banner :: Maybe Picture
}
  deriving (Eq, Ord, Show, Generic)

instance Default Profile where
  def = Profile "" Nothing Nothing Nothing Nothing

instance ToJSON Profile where
  toJSON (Profile username displayName about picture banner) = object
    [ "name" .= toJSON username
    , "display_name" .= toJSON displayName
    , "about" .= toJSON about
    , "picture" .= toJSON picture
    , "banner" .= toJSON banner
    ]

instance FromJSON Profile where
  parseJSON = withObject "profile" $ \e -> Profile
    <$> e .: "name"
    <*> e .:? "display_name"
    <*> e .:? "about"
    <*> e .:? "picture"
    <*> e .:? "banner"
