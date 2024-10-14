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
import Data.Bifunctor (second)
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
import Miso hiding (at, now, send, WebSocket(..))
import Miso.String (MisoString)
import qualified Miso.String as S
import MisoSubscribe (SubType (AllAtEOS, PeriodicForever, PeriodicUntilEOS), subscribe)
import ModelAction
import MyCrypto
import Nostr.Event
import Nostr.Filter
import Nostr.Keys
import Nostr.Kind
import Nostr.Network
import qualified Nostr.Network as Network
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
import Contacts
import Data.Default

start :: JSM ()
start = do
  keys@(Keys _ me _) <- loadKeys
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
          (defaultPagedModel (Since now))
          []
          (FindProfileModel "" Nothing $ (defaultPagedModel (Since now)) {factor = 2, step = 5 * nominalDay})
          (RelaysPageModel "")
          (Reactions Map.empty Map.empty)
          contacts
          Map.empty
          FeedPage
          now
          Map.empty
          Nothing
          [FeedPage]
          Map.empty
          (Map.fromList ((\r -> (r,(False,0,0))) <$> relaysList))
          Map.empty
          []
          Map.empty
          ""
          def
          me
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
    HandleWebSocket (WebSocketOpen r) -> 
      noEff $ model & #relays % at (r ^. #uri) % _Just % _1 .~ True
    HandleWebSocket (WebSocketError r e) -> 
      -- TODO: this causes a lot of view regeneration
      noEff $ model & #relays % at  (r ^. #uri) % _Just % _2 %~ (+) 1 
    HandleWebSocket (WebSocketClose r e) -> 
      noEff $ 
        model & #relays % at (r ^. #uri) % _Just % _3 %~ (+) 1
              & #relays % at (r ^. #uri) % _Just % _1 .~ False
    ReportError er ->
      let addError e es = e : take 20 es -- TODO: 20
       in noEff $
            model & #errors %~ addError er
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
            load pl [model ^. #me] -- fetch my profile
            forkJSM $ -- put actual time to model every 60 seconds
              let loop = do
                    now <- liftIO getCurrentTime
                    liftIO . sink . ActualTime $ now
                    liftIO . threadDelay . secs $ 60
                    loop
               in loop

            forkJSM $
              let loop = do
                    liftIO $ do
                      let isRunning (_, s) = any (== Running) $ Map.elems (s ^. #relaysState)
                      let showme (id, ss) = "subId=" <> T.pack (show id) <> ": " <> printState ss
                      subStates <- Map.toList <$> readMVar (nn ^. #subscriptions)
                      -- print $ ("branko-sub:Running subs:" <>) . T.intercalate "\n" $ showme <$> filter isRunning subStates
                      print $ ("branko-sub:subs:" <>) . T.intercalate "\n" $ showme <$> subStates
                    liftIO . threadDelay . secs $ 5
                    loop
               in loop
            -- forkJSM $ subscribeForRelays nn (Set.toList $ model ^. #contacts) sink
            liftIO . sink $ ShowFeed
    ShowFeed ->
      let contacts = (Set.toList $ model ^. #contacts)
          Since since = model ^. #feed % #since
          pagedFilter xos =
            \(Since s) (Until u) ->
              textNotesWithDeletes
                (Just s)
                (Just u)
                xos
          updated =
            model & #feed % #filter ?~ pagedFilter contacts
       in batchEff
            updated
            [ pure $ LoadMoreNotes #feed FeedPage,
              pure $
                StartFeedLongRunning
                  (textNotesWithDeletes (Just since) Nothing contacts)
            ]
    StartFeedLongRunning f ->
      effectSub model $
        subscribe
          nn
          PeriodicForever
          f
          FeedLongRunningProcess
          Nothing
          getEventRelayEither
    FeedLongRunningProcess rs ->
      let update er@(e, r) m =
            case ( e `elem` (fst <$> (m ^. #feedNew)),
                   e ^. #kind,
                   isJust $ m ^. #fromRelays % at e
                 ) of
              (False, TextNote, False) ->
                m & #feedNew %~ (\ers -> ers ++ [er])
                  & #fromRelays % at e ?~ Set.singleton r
              (False, TextNote, True) ->
                m & #fromRelays % at e %~ fmap (Set.insert r)
              (_, _, _) -> m
          updated = Prelude.foldr update model rs
       in noEff updated
    ShowNewNotes ->
      let updated = model & #feed % #page .~ 0 & #feedNew .~ []
       in batchEff updated [pure $ PagedNotesProcess True #feed FeedPage (model ^. #feedNew)]
    PagedNotesProcess putAtStart pml screen rs ->
      let (allNotes, enotes, eprofs) = processReceivedNotes rs
          plm = flip O.view model . (%) pml
          (_, replies) = Prelude.partition (not . isReply . fst) allNotes
          updatedNotes = bool 
           (plm #notes ++ orderByAgeAsc allNotes) 
           (orderByAgeAsc allNotes ++ plm #notes) putAtStart
          loadMore =
            length updatedNotes < plm #pageSize * plm #page + plm #pageSize
              && plm #factor < 100 -- TODO: put the number somewhere
          updated =
            model 
              & pml % #notes .~ updatedNotes
              & case loadMore of
                  True ->
                    pml % #factor %~ (* 2)
                  False ->
                    pml % #factor .~ 1
          events = fst <$> allNotes
       in effectSub updated $ \sink -> do
            load rl $ (eventId <$> events) ++ enotes
            load pl $ (pubKey <$> events) ++ eprofs
            liftIO $ do
              sink $ SubscribeForParentsOf pml screen $ (fst <$> replies)
              sink $ SubscribeForReplies $ (eventId <$> events)
              sink $ SubscribeForEmbeddedReplies enotes screen
              sink . SubscribeForEmbedded $ enotes
              when loadMore $
                sink . LoadMoreNotes pml $
                  screen
    ShowPrevious pml ->
      let newModel =
            model & pml % #page %~ \pn -> if pn > 0 then pn - 1 else pn
       in newModel <# do
            pure . ScrollTo $ "notes-container-bottom"
    ScrollTo here ->
      model <# (scrollIntoView here >> pure NoAction)
    ShowNext pml page ->
      let newModel = model & pml % #page %~ (+) 1
          f = newModel ^. pml
          needsSub =
            f ^. #pageSize * f ^. #page
              + f ^. #pageSize
              > length (f ^. #notes)
       in batchEff
            newModel
            [ pure $ ScrollTo "top-top",
              pure $ bool NoAction (LoadMoreNotes pml page) needsSub
            ]
    LoadMoreNotes pml page ->
      let pm = model ^. pml
          Since since = pm ^. #since
          newSince = addUTCTime (pm ^. #step * (-fromInteger (pm ^. #factor))) since
          newModel =
            model & pml % #since .~ Since newSince
       in effectSub newModel $ \sink -> do
            maybe
              (liftIO . print $ "[ERROR] EEempty filter in LoadMoreNotes")
              ( \filter ->
                  subscribe
                    nn
                    AllAtEOS
                    (filter (Since newSince) (Until since))
                    (PagedNotesProcess False pml page)
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
          RepliesRecvNoEmbedLoading 
          (Just $ SubState page)
          getEventRelayEither
    RepliesRecvNoEmbedLoading es -> -- don't load any embedded events present in the replies
      let (updated, _, _) = Prelude.foldr updateThreads (model ^. #threads, [], []) es
       in noEff $ model & #threads .~ updated
    SubscribeForParentsOf _ _ [] -> 
      noEff model
    SubscribeForParentsOf pml screen replies -> 
      let insert e (pmap, pids) = 
           fromMaybe (pmap, pids) $ do 
              parentEid <- findIsReplyTo e
              pure $ 
               (pmap & at parentEid .~ Just (e ^. #eventId), parentEid : pids)
          -- have a record of which parent goes with which child
          (pmap, pids) = Prelude.foldr insert (Map.empty,[]) replies
      in 
        effectSub model $
        subscribe
          nn
          PeriodicUntilEOS
          [anytimeF $ EventsWithId pids]
          (FeedEventParentsProcess pmap pml screen)
          (Just $ SubState screen)
          getEventRelayEither
    FeedEventParentsProcess pmap pml screen rs -> 
       let  (notes, enotes, eprofs) = processReceivedNotes rs 
            events = fst <$> notes
            update ec@(e,_) m = 
                maybe 
                  m
                  (\rt -> m & pml % #parents % at rt .~ Just ec)
                  (pmap ^. at (e ^. #eventId))
            updatedModel = Prelude.foldr update model notes
       in  effectSub updatedModel $ \sink -> do
            load rl $ (eventId <$> events) ++ enotes
            load pl $ (pubKey <$> events) ++ eprofs
            liftIO $ do
              sink $ SubscribeForReplies (eventId <$> events)
              sink $ SubscribeForEmbeddedReplies enotes screen
              sink $ SubscribeForEmbedded enotes
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
              m & #embedded % at (e ^. #eventId) 
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
            model & #reactions .~ Prelude.foldl processReceived reactions rs
    ReceivedProfiles rs ->
      let profiles =
            (\(xo, pro, when, rel) -> (xo, (pro, when))) <$> rs
          updated =
            model & #profiles
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
          profile = fst <$> model ^. #profiles % at (model ^. #me)
          fillMyProfile m = m & #myProfile .~ fromMaybe def profile
          isProfileEdit = page == MyProfilePage
       in noEff $ model & #page .~ page & #history %~ add page
                        & bool id fillMyProfile isProfileEdit
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
    DisplayReplyThread e -> 
     batchEff
       (model & #writeReplyTo ?~ e)
       [pure $ DisplayThread e]
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
    UpdateMaybeField l v -> noEff $ model & l .~ v
    FindProfile ->
      let pagedFilter xos =
            \(Since s) (Until u) ->
              textNotesWithDeletes
                (Just s)
                (Just u)
                xos
          xo =
            decodeNpub $
              model ^. #fpm % #findWho
          updated =
            model 
              & #fpm % #lookingFor .~ xo
              & #fpm % #events % #notes .~ []
              & #fpm % #events % #filter .~ (xo >>= \xo' -> Just (pagedFilter [xo']))
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
      let isRunning (SubRunning _) = True -- TODO: rewrite all these using Prisms when TH is ready
          isRunning _ = False
          isError (Network.Error _) = True
          isError _ = False
          extract (Network.Error e) = Just e
          extract _ = Nothing
          -- timeouted relays, errored relays
          (toRels, erRels) =
            case st of
              (_, SubFinished rs) ->
                -- find timeouted relays
                let trs = fst <$> filter ((== Running) . snd) (Map.toList rs)
                    ers = filter (isError . snd) (Map.toList rs)
                 in (trs, second extract <$> ers)
              _ -> ([], [])
          updateListWith (sid, ss) list =
            -- update "sub state" for sid and remove all finished "sub states"
            (sid, ss) : filter (\(sid2, ss1) -> sid2 /= sid && isRunning ss1) list
          updatedModel =
            model & #subscriptions % at p
              %~ Just
              . fromMaybe [st]
              . fmap (updateListWith st)
       in batchEff updatedModel $
            pure . ReportError
              <$> ((\r -> "Relay " <> T.pack (show $ r ^. #uri) <> " timeouted") <$> toRels)
                ++ ( ( \(r, er) ->
                         "Relay "
                           <> T.pack (show $ r ^. #uri)
                           <> " returned error: "
                           <> (fromMaybe "" er)
                     )
                       <$> erRels
                   )
    DisplayProfilePage mxo ->
      let fpm =
            FindProfileModel
              (fromMaybe "" $ encodeBechXo =<< mxo)
              mxo
              (defaultPagedModel (Since $ model ^. #now))
       in batchEff
            (model & #fpm .~ fpm)
            [pure FindProfile, pure $ GoPage ProfilePage]
    LogReceived ers ->
      let unique = Set.toList . Set.fromList $ fst <$> ers
       in trace ("branko-log-kind10002:" <> show unique) $ noEff model
    LogConsole what ->
      model <# do
        liftIO (print what) >> pure NoAction
    SendReplyTo e -> do
        let replyEventF = 
             \t -> createReplyEvent e t (model ^. #me) 
               $ model ^. #noteDraft
            localhost = Relay "localhost" (RelayInfo False False) False
            successActs = [\se -> RepliesRecvNoEmbedLoading [(se, localhost)], 
                const ClearWritingReply]
        signAndSend 
              replyEventF 
              successActs
              (singleton . const . ReportError $ "Failed sending reply!")


    ClearWritingReply -> 
       noEff $ model & #writeReplyTo .~ Nothing
                     & #noteDraft .~ ""
    -- AllLoaded -> model <# do 
    --              liftIO . print $ "AllLoaded now"
    --              pure NoAction
    SendUpdateProfile -> do
            let me = model ^. #me
            let newProfileF = \now -> setMetadata (model ^. #myProfile) me now
            signAndSend 
              newProfileF 
              (singleton . const . DisplayProfilePage $ Just me) 
              (singleton . const . ReportError $ "Failed updating profile!")
            -- TODO: update profile in #profiles if sending successfull
    _ -> noEff model
  where
    signAndSend ueF successActs failureActs = 
      effectSub model $ \sink -> do
          now <- liftIO getCurrentTime
          key <- liftIO $ getSecKey xo
          let Keys _ xo _ = keys $ nn
              signed = signEvent (ueF now) key xo
          maybe
            (liftIO . sink $ ReportError "Failed sending: Event signing failed")
            ( \se -> liftIO $ do
                isSuccess <- runNostr nn $ sendAndWait se (Seconds 1)
                bool 
                  (sequence_ ((\f -> sink (f se)) <$> failureActs)) 
                  (sequence_ ((\f -> sink (f se)) <$> successActs))
                  isSuccess
            )
            signed
      where 
       getSecKey xo = pure $ Nostr.Keys.secKey . keys $ nn -- TODO: don't store keys in NostrNetwork

    sendAndWait :: Event -> Seconds -> ReaderT NostrNetwork IO Bool
    sendAndWait e (Seconds timeout) =
      do
        let signed = e
            eid = e ^. #eventId
        RP.sendEvent signed
        let loop =
              do
                now <- liftIO getCurrentTime
                results <- getResults eid
                case results of
                  Just (res, start) ->
                    case ( checkResult res > 0.5,
                           (realToFrac $ diffUTCTime now start) > timeout -- TODO: arbitary 0.5
                         ) of
                      (True, _) -> pure True
                      (False, True) -> pure False
                      (False, False) -> do 
                        liftIO $ sleep (Seconds 0.1) 
                        loop
                  Nothing -> pure False
        loop


    -- Note: this only works correctly when subscription is AtEOS,
    --       i.e. all events are returned at once, not periodically as they arrive
    --       This allows to handle Delete events effectivelly.
    processReceivedNotes :: [(Event, Relay)] -> ([(Event, [Content])], [EventId], [XOnlyPubKey])
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
              noteEvts
          notesAndContent = (\e -> (e, processContent e)) <$> notes
          embedded = filterBech . concat $ snd <$> notesAndContent
          (eprofs, enotes) = partitionBechs embedded
       in (notesAndContent, eprofs, enotes)

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
      replyTo = maybe [] singleton $ findIsReplyTo e 
  subscribe
    nn
    PeriodicUntilEOS
    [anytimeF $ LinkedEvents eids, anytimeF $ EventsWithId (eids ++ replyTo)]
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
loadContacts = fromRight defaultContacts <$> getLocalStorage "my-contacts"

defaultContacts :: [XOnlyPubKey]
defaultContacts = someContacts

secs :: Int -> Int
secs = (* 1000000)

appView :: Model -> View Action
appView m =
  -- div_ [onLoad AllLoaded] $
  div_ [] $
    [ div_
        [ bool
            (class_ "remove-element")
            (class_ "visible")
            $ areSubsRunning m (m ^. #page)
        ]
        [loadingBar],
      newNotesIndicator,
      div_
        [class_ "main-container", id_ "top-top"]
        [ leftPanel m,
          middlePanel m,
          rightPanel m
        ],
      footerView m
    ]
  where
    howMany = length $ m ^. #feedNew
    newNotesIndicator =
      div_
        [ class_ "new-notes",
          onClick ShowNewNotes,
          bool
            (class_ "remove-element")
            (class_ "visible")
            (howMany > 0 && m ^. #page == FeedPage)
        ]
        [ span_
            [class_ "new-notes-count"]
            [ text . T.pack $
                "Display " <> show howMany <> " new " <> bool "note" "notes" (howMany > 1)
            ]
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
        [ 
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
displayFeed m =
  div_
    [class_ "feed"]
    [displayPagedNotes m #feed FeedPage]

displayPagedNotes :: Model -> (Lens' Model PagedNotesModel) -> Page -> View Action
displayPagedNotes m pml screen =
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
              onClick (ShowPrevious pml)
            ]
            [text "=<<"]
        ],
      div_
        [class_ "notes-container"]
        (displayPagedNote m pml <$> notes), -- TODO: ordering can be different
      div_
        [class_ "load-next-container"]
        [span_ [class_ "load-next", onClick (ShowNext pml screen)] [text ">>="]],
      div_ [id_ "notes-container-bottom"] []
    ]
  where
    f = m ^. pml
    pageSize = f ^. #pageSize
    page = f ^. #page
    -- notes = take (pageSize * page + pageSize) $ f ^. #notes
    notes = take pageSize . drop (page * pageSize) $ f ^. #notes

areSubsRunning :: Model -> Page -> Bool
areSubsRunning m p =
  fromMaybe False $ do
    subs <- m ^. #subscriptions % at p
    let isRunning (_, SubRunning _) = True
        isRunning (_, _) = False
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
            ++ [replyIcon]
        )
    ]
  where
    eid = e ^. #eventId
    isThreadOf = m ^. #page == ThreadPage e
    reactions = m ^. #reactions % #processed % at eid
    reid = RootEid $ fromMaybe eid $ findRootEid e
    replies = do
      thread <- m ^. #threads % at reid
      Set.size <$> thread ^. #replies % at eid
    repliesCount c =
      [ div_
          [class_ "replies-count", onClick $ DisplayThread e]
          [ bool
              (text $ "▶ " <> (S.pack . show $ c))
              (text $ "▽ " <> (S.pack . show $ c))
              isThreadOf
          ]
      ]
    replyIcon = div_ [class_ "reply-icon", onClick $ DisplayReplyThread e] [text "↪"]

displayPagedNote :: Model -> (Lens' Model PagedNotesModel) -> (Event, [Content]) -> View Action
displayPagedNote m pml ec@(e,_) 
    | isJust (findIsReplyTo e) =
        let mp = m ^. pml % #parents % at (e ^. #eventId)
        in 
          div_ [class_ "parent-reply-complex"] 
           [div_ [class_ "parent"] [responseToIco, maybe emptyParent (\p -> displayNote m p) mp]
           ,div_ [class_ "child"] [displayNote m ec]]
    | otherwise = 
        displayNote m ec
  where 
    emptyParent = div_ [] [text "Loading parent event"]
    responseToIco = div_ [class_ "responseto-icon"] 
      []

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
    profileName = span_ [class_ "username"] [text $ profile ^. #username]
    displayName =
      span_
        [class_ "display-name"]
        [text . fromMaybe "" $ profile ^. #displayName]
    noteAge = span_ [class_ "note-age"] [text . S.pack $ eventAge (m ^. #now) e]

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
      MyProfilePage -> displayMyProfilePage m

rightPanel :: Model -> View Action
rightPanel m = ul_ [class_ "right-panel"] errors
  where
    errors =
      ( \(i, e) ->
          liKeyed_
            (Key . T.pack $ show i) -- so that Miso diff algoritm displays it in the correct order
            [ class_ "error",
              class_ "hide-after-period"
            ]
            [text e]
      )
        <$> zip [1 ..] (m ^. #errors)

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
          pItem "Relays" RelaysPage,
          pItem "My Profile" MyProfilePage
          -- pItem "Bookmarks"
        ],
      div_ [class_ "logged-in-profile"] $
        [div_ [] [text "logged in as: "],
         div_ [] [text . (<> "...") . T.take 20 . fromMaybe "" . encodeBechXo $ m ^. #me]]
          ++ ( fromMaybe [] $
                 m ^. #profiles % at (m ^. #me)
                   >>= \(p, _) ->
                     pure $ [ div_ [] [text . T.pack . show $ p ^. #username]]
             ),
      div_
        [bool (class_ "invisible") (class_ "visible") showBack, onClick (GoBack)]
        [backArrow]
    ]
  where
    pItem label page =
      div_
        [class_ "left-panel-item"]
        [div_ [onClick (GoPage page)] [text label]]
    aItem label action =
      div_
        [class_ "left-panel-item"]
        [div_ [onClick action] [text label]]
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
        let profileName = span_ [class_ "username"] [text $ p ^. #username]
        let displayName =
              span_
                [class_ "display-name"]
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
                          div_ [] [span_ [class_ "follow-button"] [text "Following"],
                          button_ [class_ "unfollow-button", onClick (Unfollow xo)] [text "Unfollow"]]
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
      isWriteReply = Just e == m ^. #writeReplyTo
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
      writeReplyDisplay = 
        flip (bool Nothing) isWriteReply $ pure $
          div_
            []
            [ textarea_
                [ class_ "reply-text-area",
                  onInput $ UpdateField #noteDraft,
                  prop "content" $ m ^. #noteDraft
                ]
                [],
              button_ [onClick $ SendReplyTo e] [text "Send"]
            ]
      repliesDisplay = do
        thread <- m ^. #threads % at reid
        let replies = getRepliesFor thread (e ^. #eventId)
        pure $ (\r -> (div_ [class_ "reply"] [displayNote m r])) <$> replies
   in div_ [class_ "thread-container"] $
        catMaybes [parentDisplay, noteDisplay, writeReplyDisplay] 
          ++ fromMaybe [] repliesDisplay

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

displayMyProfilePage :: Model -> View Action
displayMyProfilePage m =
  div_
    [class_ "myprofile-edit"]
    [ input_
        [ class_ "input-username",
          placeholder_ "Enter Username",
          value_ $ m ^. #myProfile % #username,
          type_ "text",
          onInput $ UpdateField (#myProfile % #username)
        ],
      input_
        [ class_ "input-about",
          placeholder_ "Enter About",
          value_ . fromMaybe "" $ m ^. #myProfile % #about,
          type_ "text",
          onInput $ UpdateMaybeField (#myProfile % #about) . Just
        ], 
      input_
        [ class_ "input-about",
          placeholder_ "Enter profile pic URL",
          value_ . fromMaybe "" $ m ^. #myProfile % #picture,
          type_ "text",
          onInput $ UpdateMaybeField (#myProfile % #picture) . Just
        ],
      button_
          [class_ "update-profile-button", onClick SendUpdateProfile]
          [text "Update"]
    ]

displayRelaysPage :: Model -> View Action
displayRelaysPage m =
  div_ [class_ "relays-page"] $
    [info, relaysGrid, inputRelay]
  
  where
    info = div_ [class_ "relay-info"] [text $ "Remove relays which time out to improve loading speed"]
    
    displayRelay (r, (isConnected, errCnt, closeCnt)) =
      [ div_ [class_ "relay"] [text r],
        div_ [class_ "relay-connected"] [text $ bool "No" "Yes" isConnected],
        div_ [class_ "relay-error-count"] [text . T.pack . show $ errCnt],
        div_ [class_ "relay-close-count"] [text . T.pack . show $ closeCnt]
      ]
    
    relaysGrid =
      div_ [class_ "relays-grid"] $
        gridHeader
          ++ concat (displayRelay <$> (Map.toList $ m ^. #relays))
    
    gridHeader =
      [ div_ [] [text "Relay"],
        div_ [] [text "Connected"],
        div_ [] [text "Errors count"],
        div_ [] [text "Close count"]
      ]
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
        | otherwise = "now"
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
