{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
-- {-# LANGUAGE TemplateHaskell #-}

module Nostr.Filter
 
where

-- import Crypto.Schnorr
import Data.Aeson.Types (Pair)
-- import Data.DateTime
import Data.Text (Text, pack)
-- import qualified Data.Vector as V
import GHC.Exts (Item, fromList)
-- import Nostr.Event
-- import Nostr.Keys
import Nostr.Kind
-- import Nostr.Profile
-- import Numeric.Interval
import Prelude hiding (until)
import Data.Maybe (catMaybes)
-- import Data.Aeson.TH
import Data.Aeson
import Data.Char

data Filter
  = 
    -- MetadataFilter [XOnlyPubKey]
  -- | ContactsFilter [XOnlyPubKey]
  -- | TextNoteFilter [XOnlyPubKey]
  -- | LinkedEvents [EventId]
  -- | ParentEventOfEvent EventId -- parent event meaning one which eventId is a response to
  -- | RepliesToEvent EventId
  -- | CalendarTimeFilter
  -- | CalendarDayFilter
   AllNotes
  -- | AllMetadata
  deriving (Eq, Show, Ord)

instance ToJSON Filter where
  toJSON :: Filter -> Value
  toJSON (AllNotes) =
    object . fromList $  [ ("kinds", toJSON [TextNote]), ("limit", Number 20) ]

-- toPairs :: Filter -> [Item [Pair]]
-- toPairs AllNotes =
--   [ ("kinds", toJSON [TextNote]),
--     ("limit", Number 500)
--   ]

-- addTimeInterval :: DateTime -> [Item [Pair]]
-- addTimeInterval interval =
--   [ ("since", toJSON $ toSeconds (inf interval)),
--     ("until", toJSON $ toSeconds (sup interval) + 60)
--   ]

-- $(deriveFromJSON defaultOptions{constructorTagModifier = fmap toLower} ''Filter)
-- $(deriveFromJSON defaultOptions{constructorTagModifier = fmap toLower} ''DatedFilter)
