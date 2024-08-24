{-# LANGUAGE OverloadedStrings #-}

module Nostr.Keys where

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
    bip340sig,
    decodeHex,
    exportBip340Sig,
    exportSecKey,
    exportXOnlyPubKey,
    parseXOnlyPubKey,
  )

type ProfileName = Text

type CurrentlyActive = Bool

data Keys = Keys
  { secKey :: SecKey,
    xo :: XOnlyPubKey,
    current :: Bool
    -- , name ::  Maybe ProfileName
  }
  deriving (Eq, Show)

data UnknownXOnlyPubKey
  = ValidXOnlyPubKey XOnlyPubKey
  | InvalidXOnlyPubKey
  deriving (Eq, Show, Ord)

instance Ord Keys where
  compare k1 k2 =
    let (SecKey s1) = secKey k1 
        (SecKey s2) = secKey k2
    in 
    compare s1 s2

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

instance FromJSON UnknownXOnlyPubKey where
  parseJSON = withText "unknown XOnlyPubKey" $ \t -> do
    case textToByteStringType t parseXOnlyPubKey of
      Just xo ->
        return $ ValidXOnlyPubKey xo
      Nothing ->
        return InvalidXOnlyPubKey

instance ToJSON UnknownXOnlyPubKey where
  toJSON (ValidXOnlyPubKey xo) = toJSON xo
  toJSON _ = String ""

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

-- instance FromJSON KeyPair where
--   parseJSON = withText "KeyPair" $ \k -> do
--     case (textToByteStringType k Schnorr.secKey) of
--       Just k' -> return $ Schnorr.keyPairFromSecKey k'
--       _ -> fail "invalid key pair"

-- instance ToJSON KeyPair where
--   toJSON kp = String $ T.pack $ Schnorr.exportSecKey $ Schnorr.deriveSecKey kp

instance FromJSON Bip340Sig where
  parseJSON = withText "Bip340Sig" $ \s -> do
    case (textToByteStringType s bip340sig) of
      Just s' -> return s'
      _ -> fail "invalid schnorr sig"

instance ToJSON Bip340Sig where
  toJSON s = String $ T.pack $ exportBip340Sig s

textToByteStringType :: Text -> (ByteString -> Maybe a) -> Maybe a
textToByteStringType t f = case decodeHex t of
  Just bs -> f bs
  Nothing -> Nothing

-- initialKeys :: Keys
-- initialKeys = Keys kp xo True Nothing
--   where
--     kp = keyPairFromSecKey $ load "fef52b22d4568d9235ebf8a4f35dac54a4e748781441506e133532099dae0ded" secKey
--     xo = load "134bdeaf23fe7078d94b2836dcb748e762073d4bc274a2c188a44a3fc29df31c" parseXOnlyPubKey
--     load :: String -> (ByteString -> Maybe a) -> a
--     load s f =
--       case Schnorr.decodeHex s of
--         Just bs ->
--           case f bs of
--             Just b -> b
--             _ -> error "failed to load initial keys"
--         Nothing -> error "failed to load initial keys"

sameKeys :: Keys -> Keys -> Bool
sameKeys k1 k2 = secKey k1 == secKey k2

-- verifyActiveKeys :: [Keys] -> [Keys]
-- verifyActiveKeys [] = []
-- verifyActiveKeys ks =
--   case length filteredActive of
--     0 -> head filteredInactive : (tail $ filteredInactive)
--     1 -> head filteredActive : filteredInactive
--     _ -> head filteredActive : (disabledActive ++ filteredInactive)
--   where
--     filteredActive = filter (\(Keys _ _ active _) -> active == True) ks
--     filteredInactive = filter (\(Keys _ _ active _) -> active /= True) ks
--     disabledActive = disableKeys $ tail filteredActive

-- disableKeys :: [Keys] -> [Keys]
-- disableKeys ks = map (\(Keys kp xo _ n) -> Keys kp xo False n) ks
