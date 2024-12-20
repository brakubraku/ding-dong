{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TemplateHaskell #-}

module ModelAction where

import ContentUtils
import Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Time
import GHC.Generics
import Miso.String
import MyCrypto
import Nostr.Event
import Nostr.Filter
import Nostr.Network
import Nostr.Profile
import Nostr.Reaction
import Nostr.Relay
import Nostr.Request
import Nostr.WebSocket
import Optics
import StoredRelay
import ProfilesLoader.Types
import Miso (JSM)
import Utils (Seconds)
import Control.Concurrent (MVar)

data Action where
  RelayConnected :: RelayURI -> Action
  PagedEventsProcess :: Bool ->
                          (Lens' Model PagedEventsModel) ->
                          Page ->
                          [(Event, Relay)] ->
                          Action
  HandleWebSocket :: WebSocketAction -> Action
  ReceivedProfiles :: [ProfOrRelays] -> Action
  ReceivedReactions :: [(ReactionEvent, Relay)] -> Action
  NoAction :: Action
  StartAction :: Bool -> Action
  GoPage :: Page -> (Maybe ElementId) -> Action
  Unfollow :: XOnlyPubKey -> Action
  Follow :: XOnlyPubKey -> Action
  WriteModel :: Model -> Action
  ActualTime :: UTCTime -> Action
  DisplayThread :: Event -> Action
  DisplayReplyThread :: Event -> Action
  ThreadEvents :: Page -> [(Event, Relay)] -> Action
  ProfileEvents :: [(Event, Relay)] -> Action
  SubscribeForReplies :: [EventId] -> Action
  SubscribeForParentsOf :: (Lens' Model PagedEventsModel) ->
                             Page ->
                             [Event] ->
                             Action
  FeedEventParentsProcess :: (Map.Map EventId (Set.Set EventId)) ->
                               (Lens' Model PagedEventsModel) ->
                               Page ->
                               [(Event, Relay)] ->
                               Action
  SubscribeForEmbedded :: [EventId] -> Action
  EmbeddedEventsProcess :: [(Event, Relay)] -> Action
  GoBack :: Action
  UpdateField :: (Lens' Model a) -> a -> Action
  WriteTextToStorage :: Text -> Text -> Action
  LoadProfile :: Bool -> Bool -> XOnlyPubKey -> Page -> Action
  SubState :: Page -> (SubscriptionId, SubState) -> Action
  DisplayProfilePage :: (Maybe ElementId) -> XOnlyPubKey -> Action
  AddRelay :: Action
  ShowFeed :: Action
  ShowNext :: (Lens' Model PagedEventsModel) -> Page -> Action
  ShowPrevious :: (Lens' Model PagedEventsModel) -> Action
  LoadMoreEvents :: (Lens' Model PagedEventsModel) -> Page -> Action
  LogConsole :: String -> Action
  ScrollTo :: (Maybe Seconds) -> Text -> Action
  SubscribeForEmbeddedReplies :: [EventId] -> Page -> Action
  RepliesRecvNoEmbedLoading :: [(Event, Relay)] -> Action
  Report :: ReportType -> Text -> Action
  StartFeedLongRunning :: UTCTime -> [XOnlyPubKey] -> Action
  FeedLongRunningProcess :: [(Event, Relay)] -> Action
  ShowNewNotes :: Action
  SendReplyTo :: Event -> (JSM Text) -> Action
  AllLoaded :: Action
  SendUpdateProfile :: (JSM Profile) -> Action
  ChangeRelayActive :: Text -> Bool -> Action
  UpdatedRelaysList :: [StoredRelay] -> Action
  RemoveRelay :: Text -> Action
  Reload :: Action
  ListenToNotifs :: Action
  ShowNotifications :: Action
  SubscribeForPagedReactionsTo :: (Lens' Model PagedEventsModel) ->
                                    Page ->
                                    [ReactionEvent] ->
                                    Action
  PagedReactionsToProcess :: (Lens' Model PagedEventsModel) ->
                               Page ->
                               [(Event, Relay)] ->
                               Action
  SendLike :: Event -> Action
  LikeSent :: Event -> Action
  DisplayMyProfilePage :: Action
  DisplayWritePostPage :: Action
  CreateInitialProfile :: Action
  ShowModal :: Action
  LoadContactsOf :: XOnlyPubKey -> Page -> (Maybe (Set.Set XOnlyPubKey) -> Action) -> Action
  DisplayProfileContacts :: XOnlyPubKey -> Page -> Action
  SendPost :: (JSM Text) -> Action
  UpdateModel :: (Model -> Model) -> [JSM Action] -> Action
  UploadMyContacts :: Set.Set XOnlyPubKey -> Action
  ContactsLoaded :: Set.Set XOnlyPubKey -> Action
  DisplayThreadWithId :: EventId -> Action
 
data SubState = SubRunning (Map.Map Relay RelaySubState) | SubFinished (Map.Map Relay RelaySubState)
 deriving Eq

data ReportType = ErrorReport | SuccessReport
 deriving Eq

data Page
  = FeedPage
  | Following XOnlyPubKey
  | ThreadPage Event
  | ProfilePage XOnlyPubKey
  | FindProfilePage
  | FindEventPage
  | RelaysPage
  | MyProfilePage 
  | NotificationsPage 
  | WritePostPage
  deriving (Show, Eq, Generic, Ord)

newtype ErrorCount = ErrorCount Int 
--  deriving newtype (Num, Eq)
  deriving Eq
newtype CloseCount = CloseCount Int 
--  deriving newtype (Num, Eq)
  deriving Eq

type ElementId = Text

data Model = Model
  { feed :: PagedEventsModel,
    feedNew :: [(Event, Relay)],
    notifs :: PagedEventsModel,
    notifsNew :: [(Event, Relay)],
    findWho :: Text,
    profileEvents :: Map.Map XOnlyPubKey PagedEventsModel,
    relayInput :: Text,
    relaysList :: [StoredRelay],
    relaysStats :: Map.Map Text (Bool, ErrorCount, CloseCount), 
    reactions :: Reactions, -- TODO: what about deleted reactions?
    profiles :: Map.Map XOnlyPubKey (Profile, UTCTime),
    profileContacts :: Map.Map XOnlyPubKey (Set.Set XOnlyPubKey),
    profileRelays :: Map.Map XOnlyPubKey ([Relay], UTCTime),
    page :: Page,
    now :: UTCTime, -- don't know a better way to supply time
    threads :: Map.Map RootEid Thread,
    writeReplyTo :: Maybe Event, -- event to reply to
    history :: [(Page, Maybe ElementId)], -- page and what element to scroll to
    subscriptions ::
      Map.Map
        Page
        [(SubscriptionId, SubState)],
    embedded :: Map EventId ((Event, [Content]), Set.Set Relay),
    reports :: [(Int, ReportType, Text)],
    reportCounter :: Int,
    fromRelays :: Map Event (Set.Set Relay),
    replyDraft :: Text,
    postDraft :: Text,
    me :: XOnlyPubKey,
    subCancelButtons :: Map Text (MVar ()),
    findEventModel :: FindEventModel
  }
  deriving (Eq, Generic)

data FindEventModel = FindEventModel {
  bechEvent :: Text,
  error :: Maybe Text
} deriving (Eq, Generic)

defaultFindEventModel :: FindEventModel
defaultFindEventModel = FindEventModel "" Nothing

newtype CompactModel = CompactModel Model
-- Miso updates views whenever model changes. It uses Eq instance to determine that.
-- Here I define a custom one, so that updates don't happen unnecesarrily 
-- i.e. only data which is relevant for currently displayed page gets compared 
-- to determine if update is neccessary.
instance Eq CompactModel where
  (==) (CompactModel m1) (CompactModel m2) 
    | not . allEqual $ [eq #now, eq #subscriptions, eq #notifs, eq #notifsNew, eq #reports] = False
    | otherwise =
        if m1 ^. #page /= m2 ^. #page then False
        else 
          case m1 ^. #page of  
            -- TODO: would need heterogenous lists to get rid of eq 
            FeedPage -> allEqual $ [eq #feed, eq #feedNew, eq #profiles] ++ notesAndStuff
            Following xo -> allEqual [eq #profiles, eq (#profileContacts % at xo)]
            ThreadPage _ -> allEqual $ [eq #writeReplyTo, eq #replyDraft] ++ notesAndStuff
            ProfilePage xo -> 
              allEqual $ [eq #profiles, 
                          eq (#profileRelays % at xo), 
                          eq (#profileEvents % at xo), 
                          eq (#profileContacts % at xo),
                          eq myContacts] ++ notesAndStuff
            RelaysPage -> allEqual [eq #relaysStats, eq #relayInput, eq #relaysList]
            MyProfilePage -> allEqual $ [eq $ #profiles % at (m1 ^. #me)]
            NotificationsPage -> allEqual $ [eq #notifs, eq #notifsNew] ++ notesAndStuff
            FindProfilePage -> allEqual $ [eq #findWho]
            WritePostPage -> True
            FindEventPage -> allEqual [eq #findEventModel]
   where 
    eq :: Eq a => Lens' Model a -> Bool
    eq ls = m1 ^. ls == m2 ^. ls
    allEqual = Prelude.all (==True) 
    notesAndStuff = [eq #threads, eq #reactions, eq #embedded]
    myContacts = #profileContacts % at (m1 ^. #me)

newtype RootEid = RootEid EventId deriving (Eq, Ord)

newtype Since = Since UTCTime
  deriving (Eq)

newtype Until = Until UTCTime
  deriving (Eq)

type Threads = Map.Map RootEid Thread

data PagedEventsModel = PagedEventsModel
  { filter :: Maybe (Since -> Until -> [DatedFilter]),
    until :: Until,
    step :: NominalDiffTime,
    factor :: Integer,
    pg :: Int,
    pgStart :: Map.Map Int UTCTime, -- keep track of when each page starts
    pgSize :: Int,
    events :: [(Event, [Content])],
    fromRelays :: Map.Map EventId (Set.Set Relay), -- TODO:
    parents :: Map.Map EventId (Event, [Content]),
    -- events being reacted to by reactions in #events
    reactionEvents :: Map.Map EventId (Event, [Content]) 
  }
  deriving (Generic)

defaultPagedModel :: Until ->
  PagedEventsModel
defaultPagedModel until@(Until t) =
  PagedEventsModel
    { filter = Nothing,
      until = until,
      step = nominalDay / 6,
      factor = 1,
      pg = 0,
      pgStart = Map.fromList [(0, t)],
      pgSize = 15,
      events = [],
      fromRelays = Map.empty,
      parents = Map.empty,
      reactionEvents = Map.empty
    }

defProfEvntsModel :: XOnlyPubKey -> UTCTime -> PagedEventsModel
defProfEvntsModel xo now  = 
  defaultPagedModel (Until now)
      & #filter .~ Just (pagedFilter [xo])
      & #factor .~ 1
      & #step .~ nominalDay/2
 where 
  pagedFilter xos =
    \(Since s) (Until u) ->
      textNotesWithDeletes
      (Just s)
      (Just u)
      xos

-- TODO: alter this
instance Eq PagedEventsModel where
  f1 == f2 = 
         f1 ^. #pg == f2 ^. #pg
      && f1 ^. #factor == f2 ^. #factor
      && f1 ^. #until == f2 ^. #until
      && f1 ^. #pgSize == f2 ^. #pgSize
      && f1 ^. #step == f2 ^. #step
      && Prelude.length (f1 ^. #events) == Prelude.length (f2 ^. #events)
      && f1 ^. #parents == f2 ^. #parents
      && f1 ^. #reactionEvents == f2 ^. #reactionEvents

data RelayState = Connected | Disconnected | Error

data Thread = Thread
  { -- Mapping from event to it's replies
    replies :: Map.Map EventId (Set.Set EventId),
    -- Mapping from event to that event which it's a reply to
    parents :: Map.Map EventId EventId,
    -- We are mapping to EventId-s above
    -- (as opposed mapping directly to Event-s) because those
    -- events may not be loaded yet.
    -- Loaded events are stored in 'events' below
    events :: ProcessedEvents,
    -- keep track of what events you already proccessed
    -- and where are they all from
    fromRelays :: Map.Map EventId (Set.Set Relay)
  }
  deriving (Eq, Generic)

newThread :: Thread
newThread = Thread Map.empty Map.empty Map.empty Map.empty

type ProcessedEvents = Map.Map EventId (Event, [Content])

addToThread :: (Event, [Content]) -> Thread -> Thread
addToThread ec@(e, _) t =
  let replyToEid = findParentEventOf e
   in case replyToEid of
        Just eid ->
          t & #events % at (e ^. #eventId) ?~ ec
            & #parents % at (e ^. #eventId) .~ Just eid
            & #replies % at eid %~ 
              \mset -> Just $
                case mset of
                  Just set ->
                    Set.insert (e ^. #eventId) set
                  Nothing ->
                    Set.singleton (e ^. #eventId)
        Nothing -> -- it's a root event if it's not a reply to anything.
          t & #events % at (e ^. #eventId) ?~ ec

getRepliesFor :: Thread -> EventId -> [(Event, [Content])]
getRepliesFor t eid = fromMaybe [] $
  do
    replIds <- Set.toList <$> t ^. #replies % at eid
    let replies = catMaybes $ (\r -> Map.lookup r (t ^. #events)) <$> replIds
    pure replies


-- makePrisms ''Since