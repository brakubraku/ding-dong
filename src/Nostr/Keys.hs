{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Nostr.Keys where

import Crypto.Secp256k1 (derivePubKey, deriveXOnlyPubKey, createContext)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Exts (fromList)
import MyCrypto
  ( Bip340Sig,
    SecKey (SecKey),
    XOnlyPubKey (..),
    parseSignature,
    decodeHex,
    exportSignature,
    exportSecKey,
    exportXOnlyPubKey,
    parseXOnlyPubKey,
  )
import System.Entropy 

type ProfileName = Text

type CurrentlyActive = Bool

data Keys = Keys
  { secKey :: SecKey,
    xo :: XOnlyPubKey,
    current :: Bool
  }
  deriving (Eq, Show)

instance Ord Keys where
  compare k1 k2 =
    let (SecKey s1) = secKey k1
        (SecKey s2) = secKey k2
     in compare s1 s2

instance FromJSON Keys where
  parseJSON = withArray "Keys" $ \arr -> do
    sk <- parseJSON $ arr V.! 0
    xo <- parseJSON $ arr V.! 1
    c <- parseJSON $ arr V.! 2
    return $ Keys sk xo c

instance ToJSON Keys where
  toJSON (Keys sk xo c) =
    Array $
      fromList
        [ toJSON sk,
          toJSON xo,
          toJSON c
        ]

instance ToJSON SecKey where
  toJSON s = String . T.pack . exportSecKey $ s

instance FromJSON SecKey where
  parseJSON = withText "SecKey" $ \s ->
    case decodeHex s of
      Just key -> pure . SecKey $ key
      Nothing -> fail "SecKey fromJSON blew up"

instance Ord XOnlyPubKey where
  compare (XOnlyPubKey a) (XOnlyPubKey b) =
    compare a b

instance FromJSON XOnlyPubKey where
  parseJSON = withText "XOnlyPubKey" $ \p -> do
    case (textToByteStringType p parseXOnlyPubKey) of
      Just e -> return e
      _ -> fail "invalid XOnlyPubKey"

instance ToJSON XOnlyPubKey where
  toJSON x = String $ T.pack $ exportXOnlyPubKey x

instance FromJSON Bip340Sig where
  parseJSON = withText "Bip340Sig" $ \s -> do
    case (textToByteStringType s parseSignature) of
      Just s' -> return s'
      _ -> fail "invalid schnorr sig"

instance ToJSON Bip340Sig where
  toJSON s = String $ T.pack $ exportSignature s

textToByteStringType :: Text -> (ByteString -> Maybe a) -> Maybe a
textToByteStringType t f = case decodeHex t of
  Just bs -> f bs
  Nothing -> Nothing

generateKeys :: IO Keys
generateKeys = do
  ctx <- createContext
  secKey <- SecKey <$> getEntropy 32
  let xo = deriveXOnlyPubKey ctx . derivePubKey ctx $ secKey
      current = True
  pure Keys {..}

