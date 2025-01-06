{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Nostr.Filter
  ( DatedFilter (..),
    Filter (..),
    isAnytime,
    anytimeF,
    sinceF,
    textNotesWithDeletes,
    reactionsPagedFilter
  )
where

import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.Time.Clock
import Data.DateTime
import Data.Maybe (catMaybes)
import GHC.Exts (Item, fromList)
import MyCrypto
import Nostr.Event
import Nostr.Kind
import Prelude hiding (until)

data DatedFilter = DatedFilter
  { eventfilter :: Filter,
    since :: Maybe UTCTime,
    until :: Maybe UTCTime
  }
  deriving (Eq, Show)

data Filter
  = MetadataFilter [XOnlyPubKey]
  | ContactsFilter [XOnlyPubKey]
  | TextNoteFilter [XOnlyPubKey]
  | DeletesFilter [XOnlyPubKey]
  | LinkedEvents [EventId]
  | EventsWithId [EventId]
  | RepliesToEvent EventId
  | CalendarTimeFilter
  | CalendarDayFilter
  | AllNotes
  | AllMetadata
  | RelayListMetadata [XOnlyPubKey]
  | ReactionsTo [EventId]
  | ReactionsOf [XOnlyPubKey]
  | Mentions [XOnlyPubKey]
  deriving (Eq, Show, Ord)

instance ToJSON DatedFilter where
  toJSON :: DatedFilter -> Value
  toJSON (DatedFilter f Nothing Nothing) =
    object . fromList . toPairs $ f
  toJSON df@(DatedFilter f _ _) =
    object . fromList $ (toPairs f ++ addTimeInterval df)

toPairs :: Filter -> [Item [Pair]]
toPairs (ReactionsTo eids) =
  [ ("kinds", toJSON [Reaction]),
    ("#e", toJSON eids)
    -- ("authors", toJSON xos)
    -- ("limit", Number 1)
  ]
toPairs (ReactionsOf xos) =
  [ ("kinds", toJSON [Reaction]),
    ("authors", toJSON xos)
  ]
toPairs (Mentions xos) =
  [ ("#p", toJSON xos)
  ]
toPairs (RelayListMetadata xos) =
  [ ("kinds", toJSON [RelayList]),
    ("authors", toJSON xos)
    -- ("limit", Number 1)
  ]
toPairs (MetadataFilter xos) =
  [ ("kinds", toJSON [Metadata]),
    ("authors", toJSON xos)
    -- ("limit", Number 1)
  ]
toPairs (ContactsFilter xos) =
  [ ("kinds", toJSON [Contacts]),
    ("authors", toJSON xos)
    -- ,("limit", Number 500)
  ]
toPairs (TextNoteFilter xos) =
  [ ("kinds", toJSON [TextNote]),
    ("authors", toJSON xos)
  ]
toPairs (DeletesFilter xos) =
  [ ("kinds", toJSON [Delete]),
    ("authors", toJSON xos)
  ]
toPairs (LinkedEvents eids) =
  [ ("kinds", toJSON [TextNote]),
    -- ("limit", Number 500),
    ("#e", toJSON eids)
  ]
toPairs AllNotes =
  [ ("kinds", toJSON [TextNote]),
    ("limit", Number 10)
  ]
toPairs AllMetadata =
  [ ("kinds", toJSON [Metadata]),
    ("limit", Number 500)
  ]
toPairs (EventsWithId eids) =
  [("ids", toJSON eids)]
toPairs (RepliesToEvent eid) =
  [("#e", toJSON [eid])]
toPairs CalendarTimeFilter =
  [("kinds", toJSON [CalendarTime])]
toPairs CalendarDayFilter =
  [("kinds", toJSON [CalendarDay])]

addTimeInterval :: DatedFilter -> [Item [Pair]]
addTimeInterval (DatedFilter f since until) =
  catMaybes
    [ ("since",) . toJSON . toSeconds <$> since,
      ("until",) . toJSON . toSeconds <$> until
    ]

isAnytime :: DatedFilter -> Bool
isAnytime (DatedFilter f Nothing Nothing) = True
isAnytime (DatedFilter f _ _) = False

anytimeF :: Filter -> DatedFilter
anytimeF f = DatedFilter f Nothing Nothing

sinceF :: UTCTime -> Filter -> DatedFilter
sinceF when f = DatedFilter f (Just when) Nothing

textNotesWithDeletes ::
  Maybe UTCTime ->
  Maybe UTCTime ->
  [XOnlyPubKey] ->
  [DatedFilter]
textNotesWithDeletes since until xos =
  [ DatedFilter (TextNoteFilter xos) since until,
    -- you don't want to have "until" for Deletes,
    -- but instead take all of them until present
    DatedFilter (DeletesFilter xos) since Nothing
  ]

reactionsPagedFilter ::
  Maybe UTCTime ->
  Maybe UTCTime ->
  [XOnlyPubKey] ->
  [DatedFilter]
reactionsPagedFilter since until xos =
  [DatedFilter (ReactionsOf xos) since until, 
  DatedFilter (DeletesFilter xos) since Nothing]

-- $(deriveFromJSON defaultOptions{constructorTagModifier = fmap toLower} ''Filter)

-- $(deriveFromJSON defaultOptions{constructorTagModifier = fmap toLower} ''DatedFilter)
