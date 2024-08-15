{-# LANGUAGE FlexibleContexts  #-}

module Nostr.OtherUtils where 

import Data.String.Conversions
import Data.ByteString as BS
import Data.ByteString.Base16
import Data.Base16.Types
import Data.Text as T
import qualified Data.Text.Encoding as T

-- decodeHex :: Text -> Maybe ByteString
-- decodeHex str =
--   if isBase16 str
--     then Just . decodeBase16 . assertBase16 $ str
--     else Nothing

decodeHex :: (ConvertibleStrings a ByteString) => a -> Maybe ByteString
decodeHex str =
  if isBase16 $ cs str
    then Just . decodeBase16 $ assertBase16 $ cs str
    else Nothing

-- isBase16 :: Text -> Bool
-- isBase16 bs = (isValidBase16 . T.encodeUtf8) bs && isRight (decodeBase16Untyped bs)

-- isValidBase16 :: ByteString -> Bool
-- isValidBase16 = BS.all (`BS.elem` "0123456789abcdefABCDEF")
-- {-# INLINE isValidBase16 #-}