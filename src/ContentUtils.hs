{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module ContentUtils where

import BechUtils (Bech, decodeBech)
import Data.List hiding (words)
import Data.Maybe
import qualified Data.Text as T
import Prelude hiding (words)
import Optics
import Nostr.Event
import Nostr.Kind (Kind(TextNote))

data LinkType = Image | Other deriving (Show, Eq)

data Content = TextC [T.Text] | LinkC LinkType T.Text | NostrC Bech
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

whatLink :: T.Text -> LinkType
whatLink link =
  case T.takeEnd 4 link of
    ".gif" -> Image
    ".jpg" -> Image
    ".png" -> Image
    _ -> Other

processBech :: T.Text -> Content
processBech t = fromMaybe (TextC [t]) $ NostrC <$> decodeBech t

processContent :: Event -> [Content]
processContent e
 | e ^. #kind /= TextNote  = []
 | otherwise = 
  let processWord w =
        let isLink = case T.stripPrefix "http" w of
              Just suffix ->
                T.isPrefixOf "://" suffix || T.isPrefixOf "s://" suffix
              Nothing -> False
            isBech = T.stripPrefix "nostr:" w
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
   in foldr (processAll) [] $ T.words (e ^. #content)