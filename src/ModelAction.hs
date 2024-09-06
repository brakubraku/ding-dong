{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DisambiguateRecordFields #-}

module ModelAction where

import Data.Aeson (ToJSON)
import Data.DateTime (DateTime)
import Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Time
import GHC.Generics
import Miso.String
import MyCrypto
import Nostr.Event
import Nostr.Profile
import Nostr.Reaction
import Nostr.Relay
import Nostr.Request
import Nostr.Response
import Nostr.WebSocket
import Optics

data Action
  = RelayConnected RelayURI
  | ResponseReceived SubscriptionId [(Response, RelayURI)]
  | TextNotesAndDeletes [(Response, Relay)]
  | HandleWebSocket (WebSocket ())
  | ReceivedProfiles [(XOnlyPubKey, Profile, DateTime, Relay)]
  | ReceivedReactions [(ReactionEvent, Relay)]
  | NoAction
  | StartAction
  | GoPage Page
  | Unfollow XOnlyPubKey
  | WriteModel Model
  | ActualTime UTCTime
  | DisplayThread Event
  | ThreadEvents [(Event, Relay)]
  | SubscribeForReplies [Event]
  | GoBack

data Page = Home | Following | ThreadPage Event deriving (Show, Eq, Generic)

data Model = Model
  { textNotes :: Set.Set Event,
    reactions :: Reactions, -- TODO: what about deleted reactions?
    err :: MisoString,
    contacts :: [XOnlyPubKey],
    profiles :: Map.Map XOnlyPubKey (Profile, DateTime),
    page :: Page,
    now :: UTCTime, -- don't know a better way to supply time
    -- thread :: Map.Map RootEid Thread
    thread :: Map.Map RootEid Thread,
    history :: [Page]
  }
  deriving (Eq, Generic)

newtype RootEid = RootEid EventId deriving (Eq, Ord)

data Thread = Thread
  { -- rootId :: EventId,
    -- mapping from event to it's replies
    replies :: Map.Map EventId (Set.Set EventId),
    parents :: Map.Map EventId EventId,
    events :: EventsWithRelays
  }
  deriving (Eq, Generic)

newThread :: Thread
newThread = Thread Map.empty Map.empty Map.empty

type EventsWithRelays = Map.Map EventId (Event, Set.Set Relay)

addEvent :: (Event, Relay) -> EventsWithRelays -> EventsWithRelays
addEvent (evt, rel) ers =
  ers & at (evt ^. #eventId) %~ \x -> Just $
    case x of
      Just (e, rs) -> (e, Set.insert rel rs)
      Nothing -> (evt, Set.singleton rel)

addToThread :: (Event, Relay) -> Thread -> Thread
addToThread (e, rel) t =
  let replyToEid = findIsReplyTo e
   in case replyToEid of
        Just eid ->
          t
            & #events
            %~ addEvent (e, rel)
            & #parents
            %~ Map.insert (e ^. #eventId) eid
            & #replies
            % at eid
            %~ \mset -> Just $
              case mset of
                Just set ->
                  Set.insert (e ^. #eventId) set
                Nothing ->
                  Set.singleton (e ^. #eventId)
        Nothing -> t

getRepliesFor :: Thread -> EventId -> [Event]
getRepliesFor t eid = fromMaybe [] $
  do
    replIds <- Set.toList <$> t ^. #replies % at eid
    let replies = catMaybes $ (\r -> Map.lookup r (t ^. #events)) <$> replIds
    pure $ fst <$> replies
