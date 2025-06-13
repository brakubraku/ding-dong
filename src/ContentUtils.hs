{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module ContentUtils where

import BechUtils (Bech, decodeBech)
import Data.List hiding (words)
import Data.Maybe
import qualified Miso.String as S
import qualified Data.Text as T
import           Data.Text    (Text)
import Miso.String (MisoString)
import Prelude hiding (words)
import Optics
import Nostr.Event
import Nostr.Kind (Kind(TextNote))

data LinkType = Image | Video | Other deriving (Show, Eq)

data Content = TextC [MisoString] | LinkC LinkType MisoString | NostrC Bech
  deriving (Show, Eq)

filterBech :: [Content] -> [Bech]
filterBech cnt =
  catMaybes $ 
    ( \c ->
        case c of
          (NostrC bech) -> Just bech
          _ -> Nothing
    )
      <$> cnt

whatLink :: MisoString -> LinkType
whatLink link =
  case T.takeWhileEnd (/='.') . T.takeEnd 5 . T.toLower . T.strip  $ S.fromMisoString link of
    "gif" -> Image
    "jpg" -> Image
    "jpeg" -> Image
    "png" -> Image
    "webp" -> Image
    "mp4" -> Video
    "mov" -> Video
    _ -> Other

processBech :: MisoString -> Content
processBech t = fromMaybe (TextC [t]) $ NostrC <$> decodeBech (S.fromMisoString t)

processContent :: Event -> [Content]
processContent e
 | e ^. #kind /= TextNote  = []
 | otherwise = 
  let processWord w =
        let isLink = case S.stripPrefix "http" . S.strip $ w of
              Just suffix ->
                S.isPrefixOf "://" suffix || S.isPrefixOf "s://" suffix
              Nothing -> False
            isBech = S.stripPrefix "nostr:" . S.strip $ w
            process =
              case (isLink, isBech) of
                (True, _) -> LinkC (whatLink w) w
                (_, (Just bech)) -> processBech bech
                (_, _) -> TextC [w]
         in process
      processAll w (TextC ws : rest) =
        case processWord w of
          -- if two texts after each other then put them together
          TextC tw -> TextC (tw ++ ws) : rest
          other -> other : TextC ws : rest
      processAll w cnt =
        processWord w : cnt
      -- split it into paragraphs but preserve the newlines
   in foldr (processAll) [] . splitToWords $ e ^. #content

splitToWords :: MisoString -> [MisoString]
splitToWords content = concat . catMaybes $
   -- append \n\n to last word in a paragraph
   fmap (\(prefix, lastWord) -> prefix ++ [lastWord <> "\n\n"]) 
     <$> Data.List.unsnoc 
     -- split to paragraphs and paragraphs into words
       <$> (S.words <$>  S.splitOn "\n\n" content)
