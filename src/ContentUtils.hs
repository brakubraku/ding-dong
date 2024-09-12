{-# LANGUAGE OverloadedStrings #-}

module ContentUtils where

import Data.List hiding (words)
import qualified Data.Text as T
import Prelude hiding (words)

data LinkType = Image | Other deriving Show

data Content = TextC [T.Text] | LinkC LinkType T.Text
  deriving (Show)

whatLink :: T.Text -> LinkType
whatLink link =
  case T.takeEnd 4 link of
    ".gif" -> Image
    ".jpg" -> Image
    ".png" -> Image
    _ -> Other

process :: [Int] -> (T.Text, Int) -> [Content] -> [Content]
process ixs (t, ix) (TextC words : rest)
  | elem ix ixs =
      LinkC (whatLink t) t : TextC words : rest
  | otherwise =
      TextC (t : words) : rest
process ixs (t, ix) (x : rest)
  | elem ix ixs =
      LinkC (whatLink t) t : x : rest
  | otherwise =
      TextC [t] : x : rest
process ixs (t, ix) []
  | elem ix ixs =
      [LinkC (whatLink t) t]
  | otherwise =
      [TextC [t]]

processText :: T.Text -> [Content]
processText textContent =
  let contentWords = T.words textContent
      findLink t = T.isPrefixOf "http://" t || T.isPrefixOf "https://" t
      links = findIndices findLink $ contentWords
   in foldr (process links) [] (zip contentWords [0 ..])