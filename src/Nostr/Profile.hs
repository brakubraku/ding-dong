{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE OverloadedLabels   #-}
{-# LANGUAGE DeriveGeneric #-}

module Nostr.Profile where

import           Data.Aeson
import           Data.Default
import           Data.Text              (Text)
import GHC.Generics

type RelayURL = Text

type Username = Text

type DisplayName = Text

type About = Text

type Picture = Text

data Profile = Profile {
  username :: Text ,
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
    ]

instance FromJSON Profile where
  parseJSON = withObject "profile" $ \e -> Profile
    <$> e .: "name"
    <*> e .:? "display_name"
    <*> e .:? "about"
    <*> e .:? "picture"
    <*> e .:? "banner"
