{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}

module MyCrypto
  ( Secp.Bip340Sig(..),
    Secp.XOnlyPubKey(..),
    -- createContext,
    Secp.msg,
    signBip340,
    verifyBip340,
    Secp.secKey,
    Secp.KeyPair(..),
    
    parseXOnlyPubKey,
    exportXOnlyPubKey,
    deriveSecKey,
    exportSecKey,
    exportBip340Sig,
    keyPairFromSecKey,
    bip340sig,
    decodeHex,
    generateKeyPair
  )
where

import           Data.ByteString         (ByteString, length)
import qualified Data.ByteString.Base16  as B16
import qualified Data.Base16.Types as B16
import qualified Crypto.Secp256k1 as Secp
  ( Bip340Sig(..),
    XOnlyPubKey(..),
    createContext,
    msg,
    Msg,
    secKey,
    signBip340,
    verifyBip340,
    Ctx,
    derivePubKey,
    deriveSecKey,
    SecKey(..),
    PubKey(..),
    KeyPair(..),
    importXOnlyPubKey,
    generateKeyPair
  )
import qualified Crypto.Secp256k1.Internal.Base as Internal (exportXOnlyPubKey)
-- import qualified Crypto.Secp256k1.Internal.BaseOps as Internal (xOnlyPubKeyParse)
import Data.Text(unpack)
import System.IO.Unsafe
import Data.String.Conversions

ctx :: Secp.Ctx
ctx = unsafePerformIO Secp.createContext

exportXOnlyPubKey :: Secp.XOnlyPubKey -> String
exportXOnlyPubKey = exportText . Internal.exportXOnlyPubKey ctx

parseXOnlyPubKey :: ByteString -> Maybe Secp.XOnlyPubKey
parseXOnlyPubKey = Secp.importXOnlyPubKey ctx

deriveSecKey :: Secp.KeyPair -> Secp.SecKey
deriveSecKey = Secp.deriveSecKey ctx

generateKeyPair  = Secp.generateKeyPair ctx

keyPairFromSecKey :: Secp.SecKey -> Secp.KeyPair
keyPairFromSecKey (Secp.SecKey s) = Secp.KeyPair (s <> p)
  where
    (Secp.PubKey p) = Secp.derivePubKey ctx (Secp.SecKey s)

-- TODO: check if this works with new version of B16
exportText :: ByteString -> String
exportText = unpack . B16.extractBase16 . B16.encodeBase16

exportBip340Sig :: Secp.Bip340Sig -> String
exportBip340Sig (Secp.Bip340Sig sig) = exportText sig

exportSecKey :: Secp.SecKey -> String
exportSecKey (Secp.SecKey sec) = exportText sec

bip340sig :: ByteString -> Maybe Secp.Bip340Sig
bip340sig bs
  | Data.ByteString.length bs == 64 = Just $ Secp.Bip340Sig bs
  | otherwise = Nothing

-- TODO: passing Nothing for randomness seems common. Make sure this does not affect 
-- the signature negatively
signBip340 :: Secp.KeyPair -> Secp.Msg -> Maybe Secp.Bip340Sig
signBip340 kp msg = Secp.signBip340 ctx (deriveSecKey kp) msg Nothing 

verifyBip340 :: Secp.XOnlyPubKey -> Secp.Msg -> Secp.Bip340Sig -> Bool
verifyBip340 = Secp.verifyBip340 ctx

-- TODO: check if this works with new version of B16
decodeHex :: (ConvertibleStrings a ByteString) => a -> Maybe ByteString
decodeHex str =
  case B16.decodeBase16Untyped $ cs str of
    Right bs -> Just bs
    Left _   -> Nothing
