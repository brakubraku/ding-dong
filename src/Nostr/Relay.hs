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
    -- uri       :: URI
    uri :: RelayURI
  , info      :: RelayInfo
  , connected :: Bool
  }
  deriving (Show, Generic)

instance Eq Relay where 
  (==) = \r1 r2 -> uri r1 == uri r2

-- instance FromJSON URI where
--   parseJSON = withText "RelayURI" $ \u -> do
--     case mkURI u of
--       Just u' -> return u'
--       Nothing -> fail "invalid relay URI"

-- instance ToJSON URI where
--   toJSON u = String $ render u

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
    
relayName :: Relay -> Text
relayName = uri 

-- extractScheme :: Relay -> Text
-- extractScheme r = URI.unRText scheme
--   where
--     scheme = fromJust $ uri' ^. uriScheme
--     uri' = uri r

-- extractHostname :: Relay -> Text
-- extractHostname r =
--   URI.unRText $ fromJust $ uri' ^? uriAuthority . _Right . authHost
--   where
--     uri' = uri r

-- extractPort :: Relay -> Int
-- extractPort r =
--   case uri' ^? uriAuthority . _Right . authPort of
--     Just (Just p) -> wordToInt p
--     _ -> if extractScheme r == "wss" then 443 else 80
--   where
--     uri' = uri r

-- extractPath :: Relay -> Text
-- extractPath r =
--   case uri' ^? uriPath of
--     Just [] -> "/"
--     Just p  -> foldl (\x y -> x `append` "/" `append` y ) "" (map URI.unRText p)
--     _       -> "/"
--   where
--     uri' = uri r

sameRelay :: Relay -> Relay -> Bool
sameRelay r r' = uri r == uri r'

removeRelayFromList :: [Relay] -> Relay -> [Relay]
removeRelayFromList relayList relay = filter (\r' -> not $ relay `sameRelay` r') relayList
