{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoFieldSelectors #-}

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
import Nostr.Network

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
  | UpdateField (Lens' Model Text) Text
  | FindProfile
  | SubState Page (SubscriptionId, Map.Map Relay RelaySubState)

data Page
  = Home
  | Following
  | ThreadPage Event
  | ProfilePage XOnlyPubKey
  deriving (Show, Eq, Generic, Ord)

data Model = Model
  { textNotes :: Set.Set Event,
    reactions :: Reactions, -- TODO: what about deleted reactions?
    err :: MisoString,
    contacts :: [XOnlyPubKey],
    profiles :: Map.Map XOnlyPubKey (Profile, DateTime),
    page :: Page,
    now :: UTCTime, -- don't know a better way to supply time
    thread :: Map.Map RootEid Thread,
    history :: [Page],
    fpm :: FindProfileModel,
    subscriptions :: Map.Map Page [(SubscriptionId, Map.Map Relay RelaySubState)]
  }
  deriving (Eq, Generic)

newtype RootEid = RootEid EventId deriving (Eq, Ord)

data FindProfileModel = FindProfileModel
  { findWho :: Text,
    lookingFor :: Maybe XOnlyPubKey,
    found :: Maybe Profile
  }
  deriving (Eq, Generic)

data Thread = Thread
  { -- Mapping from event to it's replies
    replies :: Map.Map EventId (Set.Set EventId),
    -- Mapping from event to that event which it's a reply to
    parents :: Map.Map EventId EventId,
    -- We are mapping to EventId-s above
    -- (as opposed mapping directly to Event-s) because those
    -- events may not be loaded yet.
    -- Loaded events are stored in 'events' below
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
            & #parents --TODO: check this
            % at (e ^. #eventId)
            .~ Just eid
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
