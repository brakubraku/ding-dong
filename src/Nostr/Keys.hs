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
    parseXOnlyPubKey, signBip340, verifyBip340, msg, signBip340Rand,
  )
import System.Entropy 
import Data.Maybe (fromJust)
import Crypto.Secp256k1.Internal.Base (Rand32(..))

type ProfileName = Text

type CurrentlyActive = Bool

data Keys = Keys
  { secKey :: SecKey,
    xo :: XOnlyPubKey,
    current :: Bool
    -- , name ::  Maybe ProfileName
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

-- instance FromJSON KeyPair where
--   parseJSON = withText "KeyPair" $ \k -> do
--     case (textToByteStringType k Schnorr.secKey) of
--       Just k' -> return $ Schnorr.keyPairFromSecKey k'
--       _ -> fail "invalid key pair"

-- instance ToJSON KeyPair where
--   toJSON kp = String $ T.pack $ Schnorr.exportSecKey $ Schnorr.deriveSecKey kp

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

generateKeys :: IO Keys
generateKeys = do
  ctx <- createContext
  secKey <- SecKey <$> getEntropy 32
  let xo = deriveXOnlyPubKey ctx . derivePubKey ctx $ secKey
      current = True
  pure Keys {..}

testCrypto :: IO ()
testCrypto = do 
  let secKey = SecKey . fromJust .  decodeHex @Text $ "00665890a3aa84bf5963eec032f1460f07ef380cc634befc2771c046b2569da3"
  let xoKey = fromJust . parseXOnlyPubKey . fromJust . decodeHex @Text $ "411fe8e3d71be323b67ce609fc4cb3d25528b91e7c77aa71233c093b0c496b43"
  -- rand <- getEntropy 32 
  -- keys <- generateKeys
  -- let randtext = extractBase16 . encodeBase16 $ rand
  let rand = fromJust . decodeHex @Text $ "6b63df2e39a1edb186602371bd577fbc6eb7def6840151c10bb10e7019a2c0b0"
  -- putStrLn $ "branko-crypto-test-rand:" <> show (randtext)
  putStrLn $ "branko-crypto-test-sec-key:" <> show (secKey)
  putStrLn $ "branko-crypto-test-xo-key:" <> show (xoKey)
  -- putStrLn $ "branko-crypto-test-sec-key:" <> show (secKey keys)
  -- putStrLn $ "branko-crypto-test-sec-key:" <> show (exportXOnlyPubKey $ xo keys)
  -- let sig = signBip340 (secKey keys) (fromJust $ msg rand)
  let sig = signBip340 (secKey) (fromJust $ msg rand)
  -- let sig = parseSignature . fromJust . decodeHex @Text $ "3b5a7cf3c96ec4ae09e536f82de7b7ab163819d661072dab93aceec8dc6709a2b4b44ed498a6caca92fdfb13f2e1fe06487c66fd724aa5f13ea4ccd6ae152979"
  putStrLn $ "branko-crypto-test-signature:" <> show sig
  -- putStrLn $ "branko-crypto-test-result:" <> show (verifyBip340 (xo keys) (fromJust $ msg rand) (fromJust sig))
  putStrLn $ "branko-crypto-test-result:" <> show (verifyBip340 (xoKey) (fromJust $ msg rand) (fromJust sig))


testCrypto2 :: IO ()
testCrypto2 = do 
  -- let secKey = SecKey . fromJust .  decodeHex @Text $ "00665890a3aa84bf5963eec032f1460f07ef380cc634befc2771c046b2569da3"
  -- let xoKey = fromJust . parseXOnlyPubKey . fromJust . decodeHex @Text $ "411fe8e3d71be323b67ce609fc4cb3d25528b91e7c77aa71233c093b0c496b43"
  -- rand <- getEntropy 32 
  keys <- generateKeys
  -- let randtext = extractBase16 . encodeBase16 $ rand
  let rand = fromJust . decodeHex @Text $ "6b63df2e39a1edb186602371bd577fbc6eb7def6840151c10bb10e7019a2c0b0"
  -- putStrLn $ "branko-crypto-test-rand:" <> show (randtext)
  -- putStrLn $ "branko-crypto-test-sec-key:" <> show (secKey)
  -- putStrLn $ "branko-crypto-test-xo-key:" <> show (xoKey)
  putStrLn $ "branko-crypto-test-sec-key:" <> show (secKey keys)
  putStrLn $ "branko-crypto-test-xo-key:" <> show (exportXOnlyPubKey $ xo keys)
  let sig = signBip340 (secKey keys) (fromJust $ msg rand)
  -- let sig = signBip340 (secKey) (fromJust $ msg rand)
  -- let sig = parseSignature . fromJust . decodeHex @Text $ "3b5a7cf3c96ec4ae09e536f82de7b7ab163819d661072dab93aceec8dc6709a2b4b44ed498a6caca92fdfb13f2e1fe06487c66fd724aa5f13ea4ccd6ae152979"
  putStrLn $ "branko-crypto-test-signature:" <> show sig
  putStrLn $ "branko-crypto-test-result:" <> show (verifyBip340 (xo keys) (fromJust $ msg rand) (fromJust sig))
  -- putStrLn $ "branko-crypto-test-result:" <> show (verifyBip340 (xoKey) (fromJust $ msg rand) (fromJust sig))

testCrypto3 :: IO ()
testCrypto3 = do 
  let secKey = SecKey . fromJust .  decodeHex @Text $ "59b70a379f9e2e0867f5e1cfeebcdf8ed3dbddf1f1c2ea001ad85f604e865614"
  let xoKey = fromJust . parseXOnlyPubKey . fromJust . decodeHex @Text $ "55d545d87d8d6fd07bc924a2c1872e774af8ad49f3e31dadca0380277a6005d2"
  -- rand <- getEntropy 32 
  -- keys <- generateKeys
  -- let randtext = extractBase16 . encodeBase16 $ rand
  let rand = fromJust . decodeHex @Text $ "6b63df2e39a1edb186602371bd577fbc6eb7def6840151c10bb10e7019a2c0b0"
  -- putStrLn $ "branko-crypto-test-rand:" <> show (randtext)
  putStrLn $ "branko-crypto-test-sec-key:" <> show (secKey)
  putStrLn $ "branko-crypto-test-xo-key:" <> show (xoKey)
  -- putStrLn $ "branko-crypto-test-sec-key:" <> show (secKey keys)
  -- putStrLn $ "branko-crypto-test-sec-key:" <> show (exportXOnlyPubKey $ xo keys)
  -- let sig = signBip340 (secKey keys) (fromJust $ msg rand)
  let sig = signBip340 (secKey) (fromJust $ msg rand)
  -- let sig = parseSignature . fromJust . decodeHex @Text $ "7b151216691a9b0c5e4b1940e0c85d8353b9f01ba8d790a5b7ad204c89b9136b8f3064de025780769f6cfe56d6a65d4584304d9515e5a3a1c4d57ff7b88e6ee1"
  putStrLn $ "branko-crypto-test-signature:" <> show sig
  -- putStrLn $ "branko-crypto-test-result:" <> show (verifyBip340 (xo keys) (fromJust $ msg rand) (fromJust sig))
  putStrLn $ "branko-crypto-test-result:" <> show (verifyBip340 (xoKey) (fromJust $ msg rand) (fromJust sig))

testCrypto4 :: IO ()
testCrypto4 = do 
  let secKey = SecKey . fromJust .  decodeHex @Text $ "de985db99c490c0e0841e0efc58d95080cec6793c4da9c189bdc7dfb1fd41f4d"
  let xoKey = fromJust . parseXOnlyPubKey . fromJust . decodeHex @Text $ "b5ec27cac07256b2989e5cfb3f5a12cdcf05e32372e93684d57bca01f1fe0cf2"
  let msghex = fromJust . decodeHex @Text $ "1bd69c075dd7b78c4f20a698b22a3fb9d7461525c39827d6aaf7a1628be0a283"
  -- let sig2 = parseSignature . fromJust . decodeHex @Text $ "37dfa8216dada013ef20e654a650592afca5b1ee71532d2a75ce6d979fe7f02d1b274947908b3458e52928f399a4ec9adcc21fb9de291349c860db6c866a4c8f"
  let randSeedToSign = Just . Rand32 . fromJust . decodeHex @Text $ "2f24897891ea7b194dc3cc605cc46843cee84e075f31ea59561dcc83cf7f4a39"
  let sig = signBip340Rand (secKey) randSeedToSign (fromJust $ msg msghex)
  putStrLn $ "branko-crypto-test-sec-key:" <> show (secKey)
  putStrLn $ "branko-crypto-test-xo-key:" <> show (xoKey)
  -- let sig = parseSignature . fromJust . decodeHex @Text $ "7b151216691a9b0c5e4b1940e0c85d8353b9f01ba8d790a5b7ad204c89b9136b8f3064de025780769f6cfe56d6a65d4584304d9515e5a3a1c4d57ff7b88e6ee1"
  putStrLn $ "branko-crypto-test-signature:" <> show (exportSignature $ fromJust sig)
  -- putStrLn $ "branko-crypto-test-result:" <> show (verifyBip340 (xo keys) (fromJust $ msg rand) (fromJust sig))
  putStrLn $ "branko-crypto-test-result:" <> show (verifyBip340 (xoKey) (fromJust $ msg msghex) (fromJust sig))

