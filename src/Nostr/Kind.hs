module Nostr.Kind where

import Data.Aeson
import Data.Scientific

data Kind
  = Metadata
  | TextNote
  | Contacts
  | Delete
  | Boost
  | Calendar
  | CalendarTime
  | CalendarDay
  | CalendarRSVP
  | RelayList
  | Reaction
  | UnknownKind Scientific
  deriving (Eq, Show, Ord)

instance FromJSON Kind where
  parseJSON = withScientific "kind" $ \k -> do
    pure $
      case k of
        0 -> Metadata
        1 -> TextNote
        3 -> Contacts
        5 -> Delete
        6 -> Boost
        7 -> Reaction
        31922 -> CalendarTime
        31923 -> CalendarDay
        31924 -> Calendar
        31925 -> CalendarRSVP
        10002 -> RelayList
        k -> UnknownKind k

instance ToJSON Kind where
  toJSON Metadata = Number 0
  toJSON TextNote = Number 1
  toJSON Contacts = Number 3
  toJSON Delete = Number 5
  toJSON Boost = Number 6
  toJSON Reaction = Number 7
  toJSON CalendarTime = Number 31922
  toJSON CalendarDay = Number 31923
  toJSON Calendar = Number 31924
  toJSON CalendarRSVP = Number 31925
  toJSON RelayList = Number 10002
  toJSON (UnknownKind k) = Number k
