module Nostr.Kind where

import Control.Monad (mzero)
import Data.Aeson

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
  deriving (Eq, Show, Enum, Bounded, Ord)

instance FromJSON Kind where
  parseJSON = withScientific "kind" $ \k -> do
    case k of
      0 -> return Metadata
      1 -> return TextNote
      3 -> return Contacts
      5 -> return Delete
      6 -> return Boost
      7 -> return Reaction
      31922 -> return CalendarTime
      31923 -> return CalendarDay
      31924 -> return Calendar
      31925 -> return CalendarRSVP
      10002 -> return RelayList
      _ -> mzero

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
