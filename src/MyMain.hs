{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module MyMain where

import BechUtils
import ContentUtils
import Control.Concurrent
import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Bool (bool)
import Data.Either (fromRight)
import Data.List (singleton)
import qualified Data.List as Prelude
import qualified Data.Map as Map hiding (filter, foldr, singleton)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time
import Debug.Trace (trace)
import Miso hiding (at, now, send)
import Miso.String (MisoString)
import qualified Miso.String as S
import MisoSubscribe (SubType (AllAtEOS, PeriodicUntilEOS), subscribe)
import ModelAction
import MyCrypto
import Nostr.Event
import Nostr.Filter
import Nostr.Keys
import Nostr.Kind
import Nostr.Network
import Nostr.Profile
import Nostr.Reaction
import Nostr.Relay
import qualified Nostr.RelayPool as RP
import Nostr.Response
import Nostr.WebSocket
import Optics as O
import PeriodicLoader
import ProfilesLoader
import ReactionsLoader (createReactionsLoader)
import Utils

start :: JSM ()
start = do
  keys <- loadKeys
  contacts <- Set.fromList <$> loadContacts
  now <- liftIO getCurrentTime
  let relaysList =
        [ "wss://relay.nostrdice.com",
          "wss://lunchbox.sandwich.farm",
          "wss://relay.nostr.net",
          "wss://polnostr.xyz",
          "wss://relay.damus.io",
          "wss://nostr.at",
          "wss://ch.purplerelay.com"
        ]
  nn <-
    liftIO $
      initNetwork
        relaysList
        keys
  reactionsLoader <- liftIO createReactionsLoader
  profilesLoader <- liftIO createProfilesLoader
  let subs = [connectRelays nn HandleWebSocket]
      update = updateModel nn reactionsLoader profilesLoader
      initialModel =
        Model
          defaultPagedModel
          (FindProfileModel "" Nothing $ defaultPagedModel {factor = 2, step = 5 * nominalDay})
          (RelaysPageModel "")
          (Reactions Map.empty Map.empty)
          contacts
          Map.empty
          FeedPage
          now
          Map.empty
          [FeedPage]
          Map.empty
          relaysList
          Map.empty
          []
  startApp App {initialAction = StartAction, model = initialModel, ..}
  where
    events = defaultEvents
    view = appView
    mountPoint = Nothing
    logLevel = Off

updateModel ::
  NostrNetwork ->
  PeriodicLoader EventId (ReactionEvent, Relay) ->
  PeriodicLoader XOnlyPubKey (XOnlyPubKey, Profile, UTCTime, Relay) ->
  Action ->
  Model ->
  Effect Action Model
updateModel nn rl pl action model =
  case action of
    RelayError e -> noEff $ model -- TODO
    RelayTimeOut r ->
     let addError e es = e : take 20 es -- TODO: 20
     in noEff $ 
       model & #errors %~ addError ("Relay " <> T.pack (show $ r ^. #uri) <> " timeouted")
    StartAction ->
      effectSub
        model
        $ \sink ->
          do
            let runInNostr = liftIO . flip runReaderT nn
            -- wait for connections to relays having been established
            runInNostr $ RP.waitForActiveConnections (secs 2)
            forkJSM $ startLoader nn rl ReceivedReactions sink
            forkJSM $ startLoader nn pl ReceivedProfiles sink
            forkJSM $ -- put actual time to model every 60 seconds
              let loop = do
                    now <- liftIO getCurrentTime
                    liftIO . sink . ActualTime $ now
                    liftIO . threadDelay . secs $ 2
                    -- liftIO . sink . RelayTimeOut $ ((Relay $ "smash.com" <> T.pack (show now)) (RelayInfo False False) False)
                    loop
               in loop

            forkJSM $
              let loop = do
                    liftIO $ do
                      let isRunning (_, s) = any (== Running) $ Map.elems (s ^. #relaysState)
                      let showme (id, ss) = "subId=" <> T.pack (show id) <> ": " <> printState ss
                      subStates <- Map.toList <$> readMVar (nn ^. #subscriptions)
                      print $ ("branko-sub:Running subs:" <>) . T.intercalate "\n" $ showme <$> filter isRunning subStates
                    liftIO . threadDelay . secs $ 5
                    loop
               in loop
            -- forkJSM $ subscribeForRelays nn (Set.toList $ model ^. #contacts) sink
            liftIO . sink $ ShowFeed
    ShowFeed ->
      let updated =
            model
              & #feed
              % #filter
              ?~ pagedFilter (Set.toList $ model ^. #contacts)
       in updated <# pure (LoadMoreNotes #feed FeedPage)
    PagedNotesProcess pmLens screen rs ->
      let (notes, enotes, eprofs) = processReceivedNotes rs
          plm = flip O.view model . (%) pmLens
          updatedNotes = plm #notes ++ notes
          loadMore =
            length updatedNotes < plm #pageSize * plm #page + plm #pageSize
              && plm #factor < 100 -- TODO: put the number somewhere
          updated =
            model
              & pmLens
              % #notes
              .~ updatedNotes
              & case loadMore of
                True ->
                  pmLens % #factor %~ (* 2)
                False ->
                  pmLens % #factor .~ 1
          events = fst <$> notes
       in effectSub updated $ \sink -> do
            load rl $ (eventId <$> events) ++ enotes
            load pl $ (pubKey <$> events) ++ eprofs
            liftIO $ do
              sink $ SubscribeForReplies $ (eventId <$> events)
              sink $ SubscribeForEmbeddedReplies enotes screen
              sink . SubscribeForEmbedded $ enotes
              when loadMore $
                sink . LoadMoreNotes pmLens $
                  screen
    ShowPrevious pmLens ->
      let newModel =
            model
              & pmLens
              % #page
              %~ \pn -> if pn > 0 then pn - 1 else pn
       in newModel <# do
            pure . ScrollTo $ "notes-container-bottom"
    ScrollTo here ->
      model <# (scrollIntoView here >> pure NoAction)
    ShowNext pmLens page ->
      let newModel = model & pmLens % #page %~ (+) 1
          f = newModel ^. pmLens
          needsSub =
            f ^. #pageSize * f ^. #page
              + f ^. #pageSize
              > length (f ^. #notes)
       in batchEff
            newModel
            [ pure $ ScrollTo "top-top",
              pure $ bool NoAction (LoadMoreNotes pmLens page) needsSub
            ]
    LoadMoreNotes pmLens page ->
      let pm = model ^. pmLens
          Since since = fromMaybe (Since $ model ^. #now) $ pm ^. #since
          newSince = trace ("brankoLoading with factor=" <> show (pm ^. #factor)) $ addUTCTime (pm ^. #step * (-fromInteger (pm ^. #factor))) since
          newModel =
            model
              & pmLens
              % #since
              ?~ Since newSince
       in effectSub newModel $ \sink -> do
            maybe
              (liftIO . print $ "[ERROR] Empty filter in LoadMoreNotes")
              ( \filter ->
                  subscribe
                    nn
                    AllAtEOS
                    (filter (Since newSince) (Until since))
                    (PagedNotesProcess pmLens page)
                    (Just $ SubState page)
                    getEventRelayEither
                    sink
              )
              (pm ^. #filter)
    SubscribeForReplies [] -> noEff model
    SubscribeForReplies eids ->
      effectSub model $ subscribeForEventsReplies nn eids FeedPage
    SubscribeForEmbeddedReplies [] _ -> noEff $ model
    SubscribeForEmbeddedReplies eids page ->
      effectSub model $
        subscribe
          nn
          PeriodicUntilEOS
          [anytimeF $ LinkedEvents eids]
          EmbeddedRepliesRecv
          (Just $ SubState page)
          process
      where
        process (resp, rel) =
          maybe (Left "could not extract Event") Right $ do
            evt <- getEvent resp
            pure (evt, rel)
    EmbeddedRepliesRecv es ->
      let (updated, _, _) = Prelude.foldr updateThreads (model ^. #threads, [], []) es
       in noEff $ model & #threads .~ updated
    SubscribeForEmbedded [] ->
      noEff model
    SubscribeForEmbedded eids ->
      effectSub model $
        subscribe
          nn
          AllAtEOS
          [anytimeF $ EventsWithId eids]
          EmbeddedEventsProcess
          (Just $ SubState FeedPage)
          getEventRelayEither
    EmbeddedEventsProcess es ->
      let process :: (Event, Relay) -> (Model, Set.Set XOnlyPubKey) -> (Model, Set.Set XOnlyPubKey)
          process (e, rel) (m, xos) =
            (,Set.insert (e ^. #pubKey) xos) $
              m
                & #embedded
                % at (e ^. #eventId)
                %~ Just
                . maybe
                  ((e, processContent e), Set.singleton rel)
                  (\(ec, rels) -> (ec, Set.insert rel rels))
          (updated, xos) = Prelude.foldr process (model, Set.empty) es
       in updated <# do
            load pl $ Set.toList xos
            pure NoAction
    ReceivedReactions rs ->
      let reactions = model ^. #reactions
       in noEff $
            model
              & #reactions
              .~ Prelude.foldl processReceived reactions rs
    ReceivedProfiles rs ->
      let profiles =
            (\(xo, pro, when, rel) -> (xo, (pro, when))) <$> rs
          updated =
            model
              & #profiles
              %~ Map.unionWith
                ( \p1@(_, d1) p2@(_, d2) ->
                    -- prefer most recent profile
                    if d1 > d2 then p1 else p2
                )
                (Map.fromList profiles)
       in noEff $ updated
    GoPage page ->
      let add p ps@(p1 : _) =
            bool (p : ps) ps (p1 == p)
          add p [] = [p]
       in noEff $ model & #page .~ page & #history %~ add page
    GoBack ->
      let updated = do
            (_, xs) <- uncons $ model ^. #history
            (togo, rest) <- uncons xs
            pure $ model & #page .~ togo & #history .~ (togo : rest)
       in noEff $ fromMaybe model updated
    Unfollow xo ->
      let updated = model & #contacts % at xo .~ Nothing
       in updated
            <# (updateContacts (updated ^. #contacts) >> pure NoAction)
    Follow xo ->
      let updated = model & #contacts % at xo .~ Just ()
       in updated
            <# (updateContacts (updated ^. #contacts) >> pure NoAction)
    WriteModel m ->
      model <# (writeModelToStorage m >> pure NoAction)
    ActualTime t -> do
      noEff $ model & #now .~ t
    DisplayThread e -> do
      effectSub model $ \sink -> do
        forkJSM $ subscribeForWholeThread nn e (ThreadPage e) sink
        liftIO $ do
          sink . GoPage $ ThreadPage e
          sink . ScrollTo $ "top-top"
    ThreadEvents [] _ -> noEff $ model
    ThreadEvents es screen ->
      let (updated, enotes, eprofs) = Prelude.foldr updateThreads (model ^. #threads, [], []) es
       in effectSub (model & #threads .~ updated) $ \sink -> do
            load rl $ (eventId . fst <$> es) ++ enotes
            load pl $ (pubKey . fst <$> es) ++ eprofs
            liftIO $ do
              sink . SubscribeForEmbedded $ enotes
              sink $ SubscribeForEmbeddedReplies enotes screen
    UpdateField l v -> noEff $ model & l .~ v
    FindProfile ->
      let xo =
            decodeNpub $
              model ^. #fpm % #findWho
          updated =
            model
              & #fpm
              % #lookingFor
              .~ xo
              & #fpm
              % #events
              % #notes
              .~ []
              & #fpm
              % #events
              % #filter
              .~ (xo >>= \xo' -> Just (pagedFilter [xo']))
          runSubscriptions = do
            xo' <- xo
            pure $
              effectSub updated $ \sink -> do
                subscribe
                  nn
                  PeriodicUntilEOS
                  [DatedFilter (MetadataFilter [xo']) Nothing Nothing]
                  ReceivedProfiles
                  (Just . SubState $ ProfilePage)
                  extractProfile
                  sink
                liftIO . sink $ LoadMoreNotes (#fpm % #events) ProfilePage
       in fromMaybe
            (noEff $ updated)
            runSubscriptions
    SubState p st ->
      let isRunning rs = any (== Running) . Map.elems $ rs
          updateListWith (sid, ss) list =
            -- update "sub state" for sid and remove all finished "sub states"
            (sid, ss) : filter (\(sid2, rs) -> sid2 /= sid && isRunning rs) list
          updatedModel =
            model
              & #subscriptions
              % at p
              %~ Just
              . fromMaybe [st]
              . fmap (updateListWith st)
       in updatedModel <# pure NoAction
    DisplayProfilePage mxo ->
      let fpm =
            FindProfileModel
              (fromMaybe "" $ encodeBechXo =<< mxo)
              mxo
              defaultPagedModel
       in batchEff
            (model & #fpm .~ fpm)
            [pure FindProfile, pure $ GoPage ProfilePage]
    LogReceived ers ->
      let unique = Set.toList . Set.fromList $ fst <$> ers
       in trace ("branko-log-kind10002:" <> show unique) $ noEff model
    LogConsole what ->
      model <# do
        liftIO (print what) >> pure NoAction
    _ -> noEff model
  where
    -- Note: this only works correctly when subscription is AtEOS,
    --       i.e. all events are returned at once, not periodically as they arrive
    --       This allows to handle Delete events effectivelly.
    processReceivedNotes rs =
      let evts =
            Set.toList . Set.fromList $ fst <$> rs -- fromList, toList, to eliminate duplicates
          (noteEvts, deleteEvts) =
            Prelude.partition
              (\e -> e ^. #kind == TextNote)
              evts
          deletions =
            catMaybes $
              deleteEvts
                & fmap
                  ( \e -> do
                      ETag eid _ _ <- getSingleETag e
                      pure (e ^. #pubKey, eid)
                  )
          notes =
            filter
              ( \e ->
                  not $
                    (e ^. #pubKey, e ^. #eventId)
                      `elem` deletions
              )
              -- show original posts only (not replies)
              (filter (not . isReply) noteEvts) -- TODO:
              -- content e = processContent e
          notesAndContent = (\e -> (e, processContent e)) <$> orderByAgeAsc notes
          embedded = filterBech . concat $ snd <$> notesAndContent
          (eprofs, enotes) = partitionBechs embedded
       in (notesAndContent, eprofs, enotes)
    pagedFilter xos =
      \(Since s) (Until u) ->
        textNotesWithDeletes
          (Just s)
          (Just u)
          xos

    updateThreads :: (Event, Relay) -> (Threads, [EventId], [XOnlyPubKey]) -> (Threads, [EventId], [XOnlyPubKey])
    updateThreads (e, rel) (ts, eids, xos) =
      -- if there is no root eid in tags then this is a "top-level" note
      -- and so it's eid is the root of the thread
      let reid = RootEid $ fromMaybe (e ^. #eventId) $ findRootEid e
          thread = fromMaybe newThread $ ts ^. at reid
          updatedWithRelay =
            thread & #fromRelays % at (e ^. #eventId) %~ \mrels ->
              Just $ case mrels of
                Just rels -> Set.insert rel rels
                Nothing -> Set.singleton rel
          updatedWithRelayAndEvent =
            addToThread (e, cnt) updatedWithRelay
          cnt = processContent e
          (enotes, eprofs) = partitionBechs . filterBech $ cnt
       in case thread ^. #fromRelays % at (e ^. #eventId) of
            Just _ ->
              (ts & at reid ?~ updatedWithRelay, eids, xos)
            Nothing ->
              (ts & at reid ?~ updatedWithRelayAndEvent, enotes ++ eids, eprofs ++ xos)

-- subscriptions below are parametrized by Page. The reason is
-- so that one can within that page track the state (Running, EOS)
-- of those subscriptions
subscribeForWholeThread :: NostrNetwork -> Event -> Page -> Sub Action
subscribeForWholeThread nn e page sink = do
  let eids = [(e ^. #eventId)]
  subscribe
    nn
    PeriodicUntilEOS
    [anytimeF $ LinkedEvents eids, anytimeF $ EventsWithId eids]
    (flip ThreadEvents page)
    (Just $ SubState page)
    process
    sink
  where
    process (resp, rel) =
      maybe (Left "could not extract Event") Right $ do
        evt <- getEvent resp
        pure (evt, rel)

subscribeForEventsReplies :: NostrNetwork -> [EventId] -> Page -> Sub Action
subscribeForEventsReplies _ [] _ _ = pure ()
subscribeForEventsReplies nn eids page sink =
  -- TODO: this subscribes for whole threads for all of those eids. What you need is a lighter query which only gets the replies
  --       Seems like there is no protocol support for only subscribe to Reply e tags. You always subscribe for both Reply and Root e tags.
  --  this makes queries which only want replies (and not root replies) to a single event possibly very inefficient
  subscribe
    nn
    PeriodicUntilEOS
    [anytimeF $ LinkedEvents eids]
    (flip ThreadEvents page)
    (Just $ SubState page)
    process
    sink
  where
    process (resp, rel) =
      maybe (Left "could not extract Event") Right $ do
        evt <- getEvent resp
        pure (evt, rel)

writeModelToStorage :: Model -> JSM ()
writeModelToStorage m = pure ()

updateContacts :: Set.Set XOnlyPubKey -> JSM ()
updateContacts xos = do
  setLocalStorage "my-contacts" $ Set.toList xos

loadContacts :: JSM [XOnlyPubKey]
loadContacts = fromRight [] <$> getLocalStorage "my-contacts"

secs :: Int -> Int
secs = (* 1000000)

appView :: Model -> View Action
appView m =
  div_ [] $
    [ div_
        [ bool
            (class_ "remove-element")
            (class_ "visible")
            $ areSubsRunning m (m ^. #page)
        ]
        [loadingBar],
      div_
        [class_ "main-container", id_ "top-top"]
        [ leftPanel m,
          middlePanel m,
          rightPanel m
        ],
      footerView m
    ]

followingView :: Model -> View Action
followingView m@Model {..} =
  div_
    [class_ "following-container"]
    $ displayProfile <$> loadedProfiles
  where
    loadedProfiles :: [(XOnlyPubKey, Profile)]
    loadedProfiles =
      ( \xo ->
          let p = fromMaybe (emptyP xo) $ fst <$> (m ^. #profiles % at xo)
           in (xo, p)
      )
        <$> Set.toList contacts
      where
        -- in case profile was not found on any relay display pubKey in about
        emptyP xo = Profile "" Nothing (encodeBechXo xo) Nothing Nothing

    displayProfile :: (XOnlyPubKey, Profile) -> View Action
    displayProfile (xo, p) =
      div_
        [class_ "profile"]
        [ button_
            [class_ "unfollow-button", onClick (Unfollow xo)]
            [text "Unfollow"],
          div_
            [class_ "pic-container"]
            [displayProfilePic xo $ p ^. #picture],
          div_
            [class_ "info-container"]
            [ div_ [class_ "name"] [text $ p ^. #username],
              div_ [class_ "about"] [text . fromMaybe "" $ p ^. #about]
            ]
        ]

displayFeed ::
  Model ->
  View Action
displayFeed m = displayPagedNotes m #feed FeedPage

displayPagedNotes :: Model -> (Lens' Model PagedNotesModel) -> Page -> View Action
displayPagedNotes m pmLens screen =
  div_
    []
    [ div_ [id_ "notes-container-top"] [],
      div_
        [ class_ "load-previous-container",
          bool
            (class_ "remove-element")
            (class_ "visible")
            (page > 0)
        ]
        [ span_
            [ class_ "load-previous",
              onClick (ShowPrevious pmLens)
            ]
            [text "=<<"]
        ],
      div_
        [class_ "notes-container"]
        (displayNote m <$> notes), -- TODO: ordering can be different
      div_
        [class_ "load-next-container"]
        [span_ [class_ "load-next", onClick (ShowNext pmLens screen)] [text ">>="]],
      div_ [id_ "notes-container-bottom"] []
    ]
  where
    f = m ^. pmLens
    pageSize = f ^. #pageSize
    page = f ^. #page
    -- notes = take (pageSize * page + pageSize) $ f ^. #notes
    notes = take pageSize . drop (page * pageSize) $ f ^. #notes

areSubsRunning :: Model -> Page -> Bool
areSubsRunning m p =
  fromMaybe False $ do
    subs <- m ^. #subscriptions % at p
    let isRunning (_, s) = any (== Running) $ Map.elems s
    pure . (> 0) . length . filter isRunning $ subs

footerView :: Model -> View action
footerView Model {..} =
  div_
    [class_ "footer"]
    []

displayProfilePic :: XOnlyPubKey -> Maybe Picture -> View Action
displayProfilePic xo (Just pic) =
  img_
    [ class_ "profile-pic",
      prop "src" $ pic,
      onClick $ DisplayProfilePage (Just xo)
    ]
displayProfilePic _ _ = div_ [class_ "profile-pic"] []

displayNoteContent :: Bool -> Model -> [Content] -> View Action
displayNoteContent withEmbed m content =
  let displayContent (TextC textWords) =
        text . T.unwords $ textWords
      displayContent (LinkC Image link) =
        div_
          []
          [ a_
              [href_ link, target_ "_blank"]
              [img_ [class_ "link-pic", prop "src" link]]
          ]
      displayContent (LinkC ContentUtils.Other link) =
        div_ [] [a_ [href_ link, target_ "_blank"] [text link]]
      displayContent (NostrC (NEvent eid)) =
        case withEmbed of
          True ->
            let embdEvnt = fst <$> m ^. #embedded % at eid
             in div_
                  [class_ "embedded-event"]
                  [ maybe
                      (text "Loading embedded event...")
                      (displayEmbeddedNote m)
                      embdEvnt
                  ]
          False ->
            text . ("smash:" <>) . fromMaybe "Failed encoding nevent" . encodeBechEvent $ eid
      displayContent (NostrC (NPub xo)) =
        case withEmbed of
          True ->
            div_
              [class_ "embedded-profile"]
              $ maybe
                [text "Loading embedded profile..."]
                (displayEmbeddedProfile xo)
                (fst <$> m ^. #profiles % at xo)
          False ->
            text . fromMaybe "Failed encoding npub" . encodeBechXo $ xo
   in div_ [class_ "note-content"] $
        displayContent <$> content
  where
    displayEmbeddedProfile :: XOnlyPubKey -> Profile -> [View Action]
    displayEmbeddedProfile xo p =
      [ div_
          [class_ "pic-container"]
          [displayProfilePic xo $ p ^. #picture],
        div_
          [class_ "info-container"]
          [ div_ [class_ "name"] [text $ p ^. #username],
            div_ [class_ "about"] [text . fromMaybe "" $ p ^. #about]
          ]
      ]

displayEmbeddedNote :: Model -> (Event, [Content]) -> View Action
displayEmbeddedNote m ec = displayNote' False m ec

displayNoteShort :: Bool -> Model -> (Event, [Content]) -> View Action
displayNoteShort withEmbed m (e, content) =
  div_
    [class_ "text-note", onClick . LogConsole $ show e]
    [ displayNoteContent withEmbed m content,
      div_
        [class_ "text-note-properties"]
        ( maybe [] repliesCount replies
            ++ [displayReactions reactions]
        )
    ]
  where
    eid = e ^. #eventId
    reactions = m ^. #reactions % #processed % at eid
    reid = RootEid $ fromMaybe eid $ findRootEid e
    replies = do
      thread <- m ^. #threads % at reid
      Set.size <$> thread ^. #replies % at eid
    repliesCount c =
      [ div_
          [class_ "replies-count", onClick $ DisplayThread e]
          [text $ "▶ " <> (S.pack . show $ c)]
      ]

displayNote :: Model -> (Event, [Content]) -> View Action
displayNote = displayNote' True

displayNote' :: Bool -> Model -> (Event, [Content]) -> View Action
displayNote' withEmbed m ec@(e, _) =
  div_
    [ class_ "note-container",
      -- TODO: is 10 enough son?
      id_ (T.take 10 . eventIdToText . getEventId $ e ^. #eventId)
    ]
    [ div_
        [class_ "profile-pic-container"]
        [displayProfilePic (e ^. #pubKey) $ picUrl m e],
      div_
        [class_ "text-note-container"]
        [ div_ [class_ "profile-info"] [profileName, displayName, noteAge],
          displayNoteShort withEmbed m ec
        ],
      div_ [class_ "text-note-right-panel"] []
    ]
  where
    profile = fromMaybe unknown $ getAuthorProfile m e
    unknown = Profile "" Nothing Nothing Nothing Nothing
    profileName = span_ [id_ "username"] [text $ profile ^. #username]
    displayName =
      span_
        [id_ "display-name"]
        [text . fromMaybe "" $ profile ^. #displayName]
    noteAge = span_ [id_ "note-age"] [text . S.pack $ eventAge (m ^. #now) e]

middlePanel :: Model -> View Action
middlePanel m =
  div_
    [class_ "middle-panel"]
    [ displayPage
    ]
  where
    displayPage = case m ^. #page of
      FeedPage -> displayFeed m
      Following -> followingView m
      ThreadPage e -> displayThread m e
      ProfilePage -> displayProfilePage m
      RelaysPage -> displayRelaysPage m

rightPanel :: Model -> View Action
rightPanel m = ul_ [class_ "right-panel"] errors
  where 
    errors = 
       -- TODO: putting long string as key is probably fine 
       -- if you only have a buch of errors
       (\e -> liKeyed_ (Key e)
         [class_ "error"
           , class_ "hide-after-period"]
         [text e]) <$> m ^. #errors 
       
leftPanel :: Model -> View Action
leftPanel m =
  div_
    [class_ "left-panel"]
    [ div_
        []
        [ pItem "Feed" FeedPage,
          -- pItem "Notifications"
          -- pItem "Followers"
          pItem "Following" Following,
          aItem "Find Profile" (DisplayProfilePage Nothing),
          pItem "Relays" RelaysPage
          -- pItem "Bookmarks"
        ],
      div_
        [bool (class_ "invisible") (class_ "visible") showBack, onClick (GoBack)]
        [backArrow]
    ]
  where
    pItem label page =
      div_
        [class_ "left-panel-item"]
        [button_ [onClick (GoPage page)] [text label]]
    aItem label action =
      div_
        [class_ "left-panel-item"]
        [button_ [onClick action] [text label]]
    showBack = (> 1) . length $ m ^. #history
    backArrow = img_ [id_ "left-arrow", prop "src" $ ("arrow-left.svg" :: T.Text)]

displayProfile :: Model -> XOnlyPubKey -> View Action
displayProfile m xo =
  let notFound =
        div_
          []
          [ text $
              "Profile of "
                <> (S.pack $ show xo)
                <> " not found."
          ]
      profileDisplay = do
        (p, _) <- m ^. #profiles % at xo
        let banner =
              p ^. #banner >>= \b ->
                pure $
                  img_ [class_ "banner-pic", prop "src" b]
        let bannerDef = div_ [class_ "banner-pic-default"] []
        let profilepic =
              p ^. #picture >>= \pic ->
                pure $
                  img_ [class_ "profile-pic", prop "src" pic]
        let profilepicDef = div_ [class_ "profile-pic-default"] []
        let profileName = span_ [id_ "username"] [text $ p ^. #username]
        let displayName =
              span_
                [id_ "display-name"]
                [text . fromMaybe "" $ p ^. #displayName]
        let notesDisplay =
              displayPagedNotes m (#fpm % #events) ProfilePage
        pure $
          div_
            []
            [ div_
                [class_ "banner"]
                [fromMaybe bannerDef banner],
              div_
                [class_ "profile-pic-container"]
                [ fromMaybe profilepicDef profilepic,
                  div_ [class_ "names"] [profileName, displayName],
                  div_
                    [class_ "follow-button-container"]
                    [ if isJust $ m ^. #contacts % at xo
                        then
                          span_ [class_ "follow-button"] [text "Following"]
                        else
                          button_
                            [class_ "follow-button", onClick (Follow xo)]
                            [text "Follow"]
                    ]
                ],
              div_
                [class_ "profile-about"]
                [ div_
                    [class_ "about"]
                    [span_ [] [text . fromMaybe "" $ p ^. #about]]
                ],
              notesDisplay
            ]
   in div_
        [class_ "profile-page"]
        [fromMaybe notFound profileDisplay]

displayThread :: Model -> Event -> View Action
displayThread m e =
  let reid = RootEid $ fromMaybe (e ^. #eventId) $ findRootEid e
      parentDisplay = do
        thread <- m ^. #threads % at reid
        parentId <- thread ^. #parents % at (e ^. #eventId)
        parent <- thread ^. #events % at parentId
        pure $ div_ [class_ "parent"] [displayNote m parent]

      noteDisplay =
        Just $
          div_
            [ class_ $
                if isJust parentDisplay
                  then "note"
                  else "note-no-parent"
            ]
            [displayNote m (e, processContent e)]
      repliesDisplay = do
        thread <- m ^. #threads % at reid
        let replies = getRepliesFor thread (e ^. #eventId)
        pure $ (\r -> (div_ [class_ "reply"] [displayNote m r])) <$> replies
   in div_ [class_ "thread-container"] $
        catMaybes [parentDisplay, noteDisplay] ++ fromMaybe [] repliesDisplay

displayReactions :: Maybe (Map.Map Sentiment (Set.Set XOnlyPubKey)) -> View action
displayReactions Nothing = div_ [class_ "reactions-container"] [text ("")]
displayReactions (Just reactions) =
  let howMany = S.pack . show . length . fromMaybe Set.empty
      likes = [span_ [class_ "like-reaction"] [text "♥ "], span_ [] [text $ howMany (reactions ^. at Like)]]
      dislikes = "🖓 " <> howMany (reactions ^. at Dislike)
      others = "Others: " <> howMany (reactions ^. at Nostr.Reaction.Other)
   in div_
        [class_ "reactions-container"]
        $ likes ++ [text (" " <> dislikes <> " " <> others)]

displayProfilePage :: Model -> View Action
displayProfilePage m =
  let npub = m ^. #fpm % #findWho
      search =
        input_
          [ class_ "input-xo",
            placeholder_ "Enter npub",
            value_ npub,
            type_ "text",
            onInput $ UpdateField (#fpm % #findWho),
            onEnter $ FindProfile
          ]
      searchButton =
        button_
          [class_ "search-box-button", onClick FindProfile]
          [text "Find"]
      ppage =
        singleton
          . displayProfile m
          <$> (m ^. #fpm % #lookingFor)
   in div_ [class_ "find-profile"] $
        [div_ [class_ "search-box"] [search, searchButton]] ++ fromMaybe [] ppage
  where
    onEnter :: Action -> Attribute Action
    onEnter action = onKeyDown $ bool NoAction action . (== KeyCode 13)

displayRelaysPage :: Model -> View Action
displayRelaysPage m =
  div_ [class_ "relays-page"] $
    [info] <> 
    ( displayRelay
        <$> m ^. #relays
    )
      ++ [inputRelay]
  where
    info = div_ [class_ "relay-info"] [text $ "Remove relays which time out to improve loading speed"]
    displayRelay r =
      div_ [class_ "relay"] [text r]
    inputRelay =
      input_
        [ class_ "input-relay",
          class_
            $ bool
              "incorrect"
              "correct"
            $ validateUrl (m ^. #relaysPage % #relay),
          -- value_ npub,
          type_ "text",
          onInput $ UpdateField (#relaysPage % #relay),
          onEnter $ AddRelay
        ]
    validateUrl _ = True -- TODO
    onEnter :: Action -> Attribute Action
    onEnter action = onKeyDown $ bool NoAction action . (== KeyCode 13)

getAuthorProfile :: Model -> Event -> Maybe Profile
getAuthorProfile m e = fst <$> m ^. #profiles % at (e ^. #pubKey)

picUrl :: Model -> Event -> Maybe MisoString
picUrl m e = do
  Profile {..} <- getAuthorProfile m e
  picture

loadKeys :: JSM Keys
loadKeys = do
  let identifier = "my-keys"
  keys <- getLocalStorage identifier
  case keys of
    Right k -> pure k
    Left _ -> do
      newKeys <- liftIO $ generateKeys
      setLocalStorage identifier newKeys
      pure newKeys

eventAge :: UTCTime -> Event -> String
eventAge now e =
  let ageSeconds =
        round $ diffUTCTime now (e ^. #created_at)
      format :: Integer -> String
      format s
        | days > 0 = show days <> "d"
        | hours > 0 = show hours <> "h"
        | minutes > 0 = show minutes <> "m"
        | otherwise = ""
        where
          days = s `div` (3600 * 24)
          hours = s `div` 3600
          minutes = s `div` 60
   in format ageSeconds

subscribeForRelays :: NostrNetwork -> [XOnlyPubKey] -> Sub Action
subscribeForRelays nn xo =
  subscribe
    nn
    PeriodicUntilEOS
    [subFilter]
    LogReceived
    Nothing
    getEventRelayEither
  where
    subFilter = anytimeF . RelayListMetadata $ xo
