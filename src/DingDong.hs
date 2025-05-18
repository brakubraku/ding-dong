{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DingDong where

import BechUtils hiding (get)
import ContentUtils
    ( Content(..),
      LinkType(Other, Image, Video),
      filterBech,
      processContent )
import Control.Concurrent
import Control.Monad (when, unless, void)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Bifunctor (second, first)
import Data.Bool (bool)
import Data.Either (fromRight)
import Data.List (singleton, uncons)
import qualified Data.List as Prelude
import qualified Data.Map as Map hiding (filter, foldr, singleton)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time
import Miso hiding (at, now, send, WebSocket(..))
import Miso.String (MisoString)
import qualified Miso.String as S
import MisoSubscribe (SubType (AllAtEOS), subscribe, isSubCanceled, cancelSub, cancelButton, SubscriptionParams(..))
import ModelAction
import MyCrypto
import Nostr.Event
import Nostr.Filter
import Nostr.Keys
import Nostr.Kind
import Nostr.Network
import qualified Nostr.Network as Network
import Nostr.Profile
import Nostr.Reaction hiding (Reaction)
import Nostr.Relay
import qualified Nostr.RelayPool as RP
import Nostr.Response
import Nostr.WebSocket
import Optics as O hiding (uncons)
import PeriodicLoader
import ProfilesLoader
import ReactionsLoader (createReactionsLoader)
import Utils
import Contacts
import Data.Default
import StoredRelay (active, relay, loadRelays, saveRelays, newActiveRelay)
import ProfilesLoader.Types (ProfOrRelays)
import Data.DateTime (fromSeconds)
import qualified Nostr.Reaction as Reaction
import Debug.Trace
import Nostr.Log (logError)
import Network.URI
import SubscriptionUtils

import qualified Miso.Components.LoadingBar as LB
import Nostr.Reaction (Reaction)
-- import qualified THBits as TH (script)
-- import qualified THBits as TH (script)
import Miso.Components.ImageWithMouseActions
-- import Control.Monad.State (State, get, put, modify, runStateT)
import Control.Monad.Trans.State.Strict (runStateT)
import Control.Monad.State.Class (put, get)
import Control.Monad.Trans.Writer.Strict (runWriter)

import Control.Monad.RWS

start :: JSM ()
start = do
  (keys@(Keys _ me _), isNewKey) <- loadKeys
  now <- liftIO getCurrentTime
  relaysList <- loadRelays
  let activeRelays = relay <$> filter active relaysList
  nn <-
    liftIO $
      initNetwork
        activeRelays  
        keys
  reactionsLoader <- liftIO createReactionsLoader
  profilesLoader <- liftIO createProfilesLoader
  lastNotifDate <- loadLastNotifTime 
  let notifsFilter =
            \(Since s) (Until u) ->
              [DatedFilter (Mentions [me]) (Just s) (Just u)]
  let subs = [connectRelays nn HandleWebSocket]
      -- update :: Action -> Effect CompactModel Action
      update a = 
        EffectCore . rws $ \r (CompactModel m) -> 
          let (EffectCore rws) = updateModel nn reactionsLoader profilesLoader a
              (a', m', w') = runRWS rws r m
          in (a', CompactModel m', w')
      initialModel =
        CompactModel $ Model 
          (defFeedEvntsModel now)
          [] 
          (defNotifEvntsModel lastNotifDate notifsFilter)
          []
          ""
          Map.empty
          Map.empty
          Map.empty
          ProfilePosts
          ""
          relaysList
          (Map.fromList ((\r -> (r,(False, ErrorCount 0,CloseCount 0))) <$> activeRelays ^.. folded % #uri))
          (Reactions Map.empty Map.empty)
          Map.empty
          Map.empty
          Map.empty
          FeedPage
          now
          Map.empty
          Nothing
          [(FeedPage,Nothing)]
          Map.empty
          []
          0
          Map.empty
          ""
          ""
          me
          Map.empty
          defaultFindEventModel
      styles = []
  startApp App {initialAction = Just $ StartAction isNewKey, model = initialModel, ..}
  where
    events = foldr Map.delete defaultEvents ["mouseup","mousedown","mouseleave", "mouseover","mouseout","mouseenter"] 
    view (CompactModel m) = appView m
    -- mountPoint = "body"
    mountPoint = Just "miso-mountpoint"
    logLevel = Off

updateModel ::
  NostrNetwork ->
  PeriodicLoader EventId (ReactionEvent, Relay) ->
  PeriodicLoader XOnlyPubKey ProfOrRelays ->
  Action ->
  Effect Model Action
updateModel nn rl pl action = do
  model <- get
  traceM $ "Running action"
  case action of
    HandleWebSocket (WebSocketOpen r) -> 
      noEff $ model & #relaysStats % at (r ^. #uri) % _Just % _1 .~ True

    HandleWebSocket (WebSocketError r e) -> 
      -- TODO: this causes a lot of view regeneration
      noEff $ 
        model & #relaysStats % at  (r ^. #uri) % _Just % _2 %~ (\(ErrorCount ec) -> ErrorCount (ec+1))

    HandleWebSocket (WebSocketClose r e) -> 
      noEff $ 
        model & #relaysStats % at (r ^. #uri) % _Just % _3 %~ (\(CloseCount cc) -> CloseCount (cc+1))
              & #relaysStats % at (r ^. #uri) % _Just % _1 .~ False

    Report reportType report ->
      let counter = model ^. #reportCounter
          add rs = (counter, reportType, report) : rs
          updated = 
            model & #reports %~ add 
                  & #reportCounter %~ (+1)
       in effectSub updated $ \sink -> 
             do 
              liftIO . sleep $ Seconds 15
              sink $ UpdateModel (removeReport counter) []
      where 
        removeReport which model = 
          model & #reports %~ filter (\(c,_,_) -> c /= which)
            
    UpdatedRelaysList rl -> 
      noEff $ model & #relaysList .~ rl

    ChangeRelayActive uri isActive -> 
      let updated = (model ^. #relaysList) &
            traversed 
            % unsafeFiltered (\r -> r ^. #relay % #uri == uri) 
            %~ O.set #active isActive
      in model <# do 
          saveRelays updated
          pure $ UpdatedRelaysList updated

    AddRelay getUri -> 
      -- let uri = model ^. #relayInput
        model <# do
          uri <- getUri 
          case parseURI . T.unpack $ uri of 
            Nothing ->
              pure $ Report ErrorReport "Invalid relay url"
            Just _ -> do 
              let nsr = newActiveRelay . newRelay $ uri
                  updated = 
                    nsr : filter 
                      (\sr -> sr ^. #relay % #uri /= uri) 
                      (model ^. #relaysList)
              saveRelays updated 
              pure $ UpdatedRelaysList updated

    RemoveRelay r -> 
      let updated = filter (\sr -> sr ^. #relay % #uri /= r) $ model ^. #relaysList
      in model <# do 
          saveRelays updated
          pure $ UpdatedRelaysList updated

    Reload -> 
      model <# do 
        reloadPage >> pure NoAction

    StartAction isNew ->
      effectSub
        model
        $ \sink ->
          do
            -- wait for connections to relays having been established
            liftIO . runInNostr $ RP.waitForActiveConnections (Seconds 2)
            forkJSM $ startLoader nn rl ReceivedReactions reportErrorAction sink
            forkJSM $ startLoader nn pl ReceivedProfiles reportErrorAction sink
            -- fetch my profile
            load pl $ [model ^. #me] 
            forkJSM $ -- put actual time to model every 60 seconds
              let loop = do
                    now <- liftIO getCurrentTime
                    sink $ ActualTime now
                    liftIO . sleep . Seconds $ 60
                    loop
               in loop
            
            forkJSM $
              let loop = do
                    liftIO $ do
                      let isRunning (_, s) = any (== Running) $ Map.elems (s ^. #relaysState)
                      let showme (id, ss) = "subId=" <> showt id <> ": " <> printState ss
                      subStates <- Map.toList <$> readMVar (nn ^. #subscriptions)
                      -- print $ ("branko-sub:Running subs:" <>) . T.intercalate "\n" $ showme <$> filter isRunning subStates
                      print $ ("branko-sub:subs:" <>) . T.intercalate "\n" $ showme <$> subStates
                    liftIO . sleep . Seconds $ 5
                    loop
               in loop
          
            storageContacts <- Set.fromList <$> loadContactsFromStorage
            sink $ GoPage FeedPage Nothing
            sink $ loadContactsFromNostr storageContacts
            when isNew $ sink CreateInitialProfile
            -- start notifications
            sink $ LoadMoreEvents #notifs NotificationsPage
            sink ListenToNotifs

          where 
            loadContactsFromNostr mine = LoadContactsOf (model ^. #me) (model ^. #page) $ uploadIfNoContacts mine
            uploadIfNoContacts :: Set.Set XOnlyPubKey -> Maybe (Set.Set XOnlyPubKey) -> Action
            uploadIfNoContacts toUpload Nothing = UploadMyContacts toUpload
            uploadIfNoContacts _ (Just cs) = ContactsLoaded cs
  
    ShowFeed ->
      let contacts = maybe [] Set.toList $ model ^. #profileContacts % at (model ^. #me)
          pagedFilter =
            \(Since s) (Until u) ->
              textNotesWithDeletes
                (Just s)
                (Just u)
                contacts
          updated =
            model & #feed .~ defFeedEvntsModel (model ^. #now)
                  & #feed % #filter ?~ pagedFilter
       in batchEff
            updated
            [ pure $ LoadMoreEvents #feed FeedPage
            , pure $ StartFeedLongRunning (model ^. #now) contacts]
    
    ShowNotifications -> 
      let updated = model & #notifs % #pg .~ 0 & #notifsNew .~ []
          new = model ^. #notifsNew
          hasNew = length new > 0
          saveLast = do 
            now <- liftIO getCurrentTime
            saveLastNotif now 
            pure NoAction
      in batchEff (bool model updated hasNew) $ 
            bool [] 
                 [pure $ PagedEventsProcess True #notifs NotificationsPage new] 
                 hasNew  
               ++ [pure $ GoPage NotificationsPage Nothing, saveLast]

    ListenToNotifs -> 
      effectSub model (void . forkJSM . runLoop)
       where 
        doSubscribe lnd sink = 
          subscribe nn sink $
           periodicForever 
            [sinceF lnd $ Mentions [model ^. #me]]
            (\ers -> UpdateModel (\m -> processNewNotifs m ers) [])
            Nothing
        runLoop sink = 
          do
            lnd <- loadLastNotifTime
            doSubscribe lnd sink 
            -- subscription will terminate when any relay returns an error
            -- and new one will be created instead. you want this with 
            -- "forever running" subscriptions, in this case to constantly check for 
            -- notifications
            waitForReconnect $ sink
            runLoop sink            
        processNewNotifs m ers =
          let update er@(e, r) m =
                m & #fromRelays % at e 
                     %~ Just . fromMaybe (Set.singleton r) . fmap (Set.insert r)
                & case (e `elem` (fst <$> m ^. #notifsNew), 
                        e `elem` (fst <$> m ^. #notifs % #events), 
                        any (== e ^. #kind) [TextNote, Reaction]) 
                  of 
                    (False, False, True) -> #notifsNew %~ (\ers' -> ers' ++ [er])
                    _ -> id   
          in Prelude.foldr update m ers

    StartFeedLongRunning since contacts ->
       effectSub model $ \sink -> 
        do
          -- cancel the existing subscription
          sequence_ $ cancelSub <$> (model ^. #subCancelButtons % at "feed-long-running")
          -- create cancel button for the new subscription
          cb <- liftIO newEmptyMVar
          let updateCBs = O.set (#subCancelButtons % at "feed-long-running") (Just cb)
          -- save cancel button for the new subscription
          sink $ UpdateModel updateCBs []
          void . forkJSM $ runLoop cb sink
       where 
        doSubscribe cb sink =
          subscribe nn sink $
            periodicForever
                (textNotesWithDeletes (Just since) Nothing contacts)
                FeedLongRunningProcess
                (Just cb)
            
        runLoop cb sink = 
          do
            doSubscribe cb sink
            isCancelled <- isSubCanceled cb
            unless isCancelled $ 
             do 
              waitForReconnect $ sink
              runLoop cb sink   

    FeedLongRunningProcess ers ->
       let update er@(e, r) m =
              m & #fromRelays % at e 
                     %~ Just . fromMaybe (Set.singleton r) . fmap (Set.insert r)
                & case (e `elem` (fst <$> m ^. #feedNew),
                        e `elem` (fst <$> m ^. #feed % #events), 
                        any (== e ^. #kind) [TextNote])
                  of 
                    (False, False, True) -> #feedNew %~ (\ers' -> ers' ++ [er])
                    _ -> id   
           updated =  Prelude.foldr update model ers
       in noEff updated

    ShowNewNotes ->
      let updated = model & #feed % #pg .~ 0 & #feedNew .~ []
       in batchEff updated [pure $ PagedEventsProcess True #feed FeedPage (model ^. #feedNew), pure $ ScrollTo Nothing "top-top"]

    PagedEventsProcess putAtStart pml screen rs ->
      let (ecs, enotes, eprofs) = processReceivedEvents rs
          plm = flip O.view model . (%) pml
          (_, replies) = Prelude.partition (not . isReply . fst) ecs
          updatedEvents = bool 
            (plm #events ++ orderByAgeAsc ecs) 
            (orderByAgeAsc ecs ++ plm #events) 
            putAtStart
          updated =
            model 
              & pml % #events .~ updatedEvents
          events = fst <$> ecs
          reactions = catMaybes $ Nostr.Reaction.extract <$> events
       in effectSub updated $ \sink -> do
            load rl $ (eventId <$> events) ++ enotes
            load pl $ (pubKey <$> events) ++ eprofs
            sink $ SubscribeForPagedReactionsTo pml screen reactions
            sink $ SubscribeForParentsOf pml screen $ (fst <$> replies)
            sink $ SubscribeForReplies $ (eventId <$> events)
            sink $ SubscribeForEmbeddedReplies enotes screen
            sink $ SubscribeForEmbedded enotes
            sink $ LoadMoreIfNecessary (castOptic pml) $ LoadMoreEvents pml screen

    ShowPrevious pml ->
      let newModel =
            model & pml % #pg %~ \pn -> if pn > 0 then pn - 1 else pn
       in newModel <# do
            pure . ScrollTo (Just $ Seconds 0.5) $ "notes-container-bottom"

    ScrollTo delay here ->
      effectSub model $ \sink -> 
        do 
        -- TODO: bit of a hack to introduce delay before scrolling
        -- so that page is hopefully fully loaded after that delay,
        -- otherwise it will scroll to elsewhere
         fromMaybe (pure ()) $ liftIO . sleep <$> delay 
         Utils.scrollIntoView here 
         sink NoAction

    ShowNext pml page ->
      let (Until start) = model ^. pml % #until
          nextPage = model ^. pml % #pg + 1
          newModel = model & pml % #pg .~ nextPage
                           & pml % #pgStart % at nextPage ?~ start
          f = newModel ^. pml
          needsSub =
            f ^. #pgSize * f ^. #pg
              + f ^. #pgSize
              > length (f ^. #events)
       in batchEff
            newModel
            [ pure $ ScrollTo Nothing "top-top",
              pure $ bool NoAction (LoadMoreEvents pml page) needsSub
            ]

    LoadMoreEvents pml page ->
      let pm = model ^. pml
          Until until = pm ^. #until
          newSince = addUTCTime (pm ^. #step * (-fromInteger (pm ^. #factor))) until
          updated =
            model & pml % #until .~ Until newSince
       in effectSub updated $ \sink -> do
            maybe
              (liftIO . print $ "[ERROR] EEempty filter in LoadMoreEvents")
              ( \filter ->
                  subscribe nn sink $ 
                      allAtEOSOnPage 
                       page
                       (filter (Since newSince) (Until until))
                       (model ^. pml % #process $ page)
                      
              )
              (pm ^. #filter)

    SubscribeForReplies [] -> noEff model
    SubscribeForReplies eids ->
      effectSub model $ subscribeForEventsReplies nn eids FeedPage

    SubscribeForEmbeddedReplies [] _ -> noEff $ model
    SubscribeForEmbeddedReplies eids page ->
      effectSub model $ \sink ->
        subscribe nn sink $
         periodicUntilEOSOnPage
          page
          [anytimeF $ LinkedEvents eids]
          RepliesRecvNoEmbedLoading 

    RepliesRecvNoEmbedLoading es -> -- don't load any embedded events present in the replies
      let (updated, _, _) = Prelude.foldr updateThreads (model ^. #threads, [], []) es
       in noEff $ model & #threads .~ updated

    SubscribeForPagedReactionsTo _ _ [] -> noEff model
    SubscribeForPagedReactionsTo pml screen res -> 
      effectSub model $ \sink ->
        subscribe nn sink $
          periodicUntilEOSOnPage
            screen
            [anytimeF . EventsWithId $ res ^.. folded % #reactionTo]
            (PagedReactionsToProcess pml screen)

    PagedReactionsToProcess pml _ ers -> 
      let process (e,_) m = 
            m & pml % #reactionEvents % at (e ^. #eventId) ?~ (e, processContent e)
          updated = Prelude.foldr process model ers
      in 
        noEff updated 
     
    SubscribeForParentsOf _ _ [] -> 
      noEff model
    SubscribeForParentsOf pml screen replies -> 
      let insert e (pmap, pids) = 
           fromMaybe (pmap, pids) $ do 
              parentEid <- findParentEventOf e
              let eid = e ^. #eventId
              pure $ 
               (pmap & at parentEid %~ -- record which parent goes with which child/children
                  Just 
                   . fromMaybe (Set.singleton eid) 
                   . fmap (Set.insert eid), parentEid : pids)
          (pmap, pids) = Prelude.foldr insert (Map.empty,[]) replies
      in 
        effectSub model $ \sink ->
          subscribe nn sink $
             periodicUntilEOSOnPage
              screen
              [anytimeF $ EventsWithId pids]
              (FeedEventParentsProcess pmap pml screen)

    FeedEventParentsProcess pmap pml screen rs -> 
       let  (notes, enotes, eprofs) = processReceivedEvents rs 
            events = fst <$> notes
            upd ec chid m = m & pml % #parents % at chid .~ Just ec
            update ec@(p,_) m = 
                maybe 
                  m
                  (\chids -> Prelude.foldr (upd ec) m chids)
                  (pmap ^. at (p ^. #eventId))
            updatedModel = Prelude.foldr update model notes
       in  effectSub updatedModel $ \sink -> do
            load rl $ (eventId <$> events) ++ enotes
            load pl $ (pubKey <$> events) ++ eprofs
            sink $ SubscribeForReplies (eventId <$> events)
            sink $ SubscribeForEmbeddedReplies enotes screen
            sink $ SubscribeForEmbedded enotes

    SubscribeForEmbedded [] ->
      noEff model
    SubscribeForEmbedded eids ->
      effectSub model $ \sink ->
        subscribe nn sink $ 
         allAtEOSOnPage
          FeedPage
          [anytimeF $ EventsWithId eids]
          EmbeddedEventsProcess
          
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
      let process :: ProfOrRelays -> Model -> Model
          process por m = 
           case por of 
            (xo, Just (profile, date, _), Nothing) -> 
              m & #profiles % at xo %~ Just .
              fromMaybe (profile, date) . fmap (\(p,d) -> do 
                  if date > d 
                  then (profile,date)
                  else (p,d))
            (xo, Nothing, Just (relays, date)) -> 
              m & #profileRelays % at xo %~ Just .
               fromMaybe (relays, date) . fmap (\(r,d) -> do 
                  if date > d 
                  then (relays,date)
                  else (r,d))
            _ -> m 
      in noEff $ Prelude.foldr process model rs 

    GoPage page elementId ->
      let add p ps@(p1 : rest) =
            bool ((p, Nothing) : (fst p1, elementId) : rest) ps (fst p1 == p)
          add p [] = [(p, Nothing)]
          updated = model & #page .~ page & #history %~ add page
       in effectSub updated $ \_ -> do 
            notify LB.loadingBar $ LB.UpdatePage page

    GoBack ->
      let updated = do
            (_, xs) <- uncons $ model ^. #history
            (prevPage, rest) <- uncons xs
            pure (model & #page .~ fst prevPage & #history .~ (prevPage : rest), snd prevPage)
       in maybe (noEff model)
           (\(updatedModel, scrollToId) -> 
              -- if there is information about the element to scroll to, then scroll to it
              updatedModel <# maybe (pure NoAction) (\eid -> pure (ScrollTo (Just $ Seconds 0.5) eid)) scrollToId)
           updated

    Unfollow xo ->
      let myContacts = #profileContacts % at (model ^. #me)
          updated = model & myContacts % _Just % at xo .~ Nothing
       in batchEff model [pure $ UploadMyContacts (fromMaybe Set.empty $ updated ^. myContacts)]

    Follow xo ->
      let myContacts = #profileContacts % at (model ^. #me)
          updated = model & myContacts % _Just % at xo .~ Just ()
       in batchEff model [pure $ UploadMyContacts (fromMaybe Set.empty $ updated ^. myContacts)]

    WriteModel m ->
      model <# (writeModelToStorage m >> pure NoAction)

    ActualTime t -> do
      noEff $ model & #now .~ t

    DisplayThread e -> do
       effectSub model $ \sink -> do
        forkJSM $ subscribeForWholeThread nn e (ThreadPage e) sink
        sink . GoPage (ThreadPage e) . Just $ getNoteElementId e
        sink . ScrollTo Nothing $ "top-top"

    DisplayReplyThread e -> 
     effectSub (model & #writeReplyTo ?~ e) $ 
      \sink -> 
        do 
         draft <- fromRight "" <$> getLocalStorage "reply-draft"
         sink . UpdateField #replyDraft $ draft
         sink $ DisplayThread e

    ThreadEvents _  [] -> noEff $ model
    ThreadEvents screen es ->
      let (updated, enotes, eprofs) = Prelude.foldr updateThreads (model ^. #threads, [], []) es
       in effectSub (model & #threads .~ updated) $ \sink -> do
            load rl $ (eventId . fst <$> es) ++ enotes
            load pl $ (pubKey . fst <$> es) ++ eprofs
            sink . SubscribeForEmbedded $ enotes
            sink $ SubscribeForEmbeddedReplies enotes screen

    UpdateField l v -> noEff $ model & l .~ v

    UploadMyContacts cs -> 
      effectSub model $ 
       \sink -> 
         do
          signAndSend 
            (pure . setContacts (Set.toList cs) (model ^. #me))
            [ const $ Report SuccessReport $ "Contacts uploaded"
            , const $ ContactsLoaded cs]
            [const $ Report ErrorReport $ "Failed uploading my contacts!"]
            sink
          safeUpdateLocalContacts cs

    ContactsLoaded cs -> 
      updated <# 
        do 
          load pl (Set.toList cs)
          pure ShowFeed
      where 
        updated = model & #profileContacts % at (model ^. #me) ?~ cs

    LoadContactsOf xo page takeAction -> 
      effectSub model $ \sink ->
          subscribe nn sink $ 
           allAtEOSOnPage
            page 
            [DatedFilter (ContactsFilter [xo]) Nothing Nothing]
            (takeAction . processReceived)
     where 
      processReceived :: [(Event, Relay)] -> Maybe (Set.Set XOnlyPubKey)
      processReceived [] = Nothing
      processReceived ers = 
        let latest =
                 -- take the latest event from each relay
              Prelude.maximumBy (\x y -> compare (x ^. #created_at) (y ^. #created_at) ) <$> fmap fst <$>
                 -- group by relays
               (Prelude.groupBy (\x y -> snd x == snd y) $
                 -- remove duplicate events received from differet relays 
                 Prelude.nubBy (\x y -> fst x == fst y) ers)
        in 
          -- extract contacts from the events
          Just . Set.fromList . Prelude.concat $ extractContacts <$> latest 
      extractContacts :: Event -> [XOnlyPubKey]
      extractContacts event = 
         let isPTag (PTag _ _ _) = True
             isPTag _ = False    
             extractXo (PTag xo _ _) = Just xo 
             extractXo _ = Nothing
         in catMaybes $ extractXo <$> event ^.. #tags % folded % filtered isPTag

    DisplayProfileContacts xo page -> 
      model <# do 
        load pl $ maybe [] Set.toList $ model ^. #profileContacts % at xo
        pure $ GoPage (Following xo) Nothing

    LoadProfileReactions xo page -> 
      let 
        reactions = defProfReactionsModel xo $ model ^. #now
      in 
        batchEff model $
         pure <$> 
          [LoadMoreEvents (#profileReactions % at xo % non reactions) page]

    ProcessProfileReactions xo page rs -> 
      let events = removeDeletetedAndDuplicates rs
          sorted = reverse $ Prelude.sort events
          processed = catMaybes $ fmap (\r -> (r, getReaction r)) . Reaction.extract <$> sorted
       
          mPm = do 
            pm <- model ^? #profileReactions % at xo % _Just
          -- put newly received at the end
            pure $ pm & #events %~ \es -> es ++ processed

          updateReactionsTo :: [(Event, Relay)] -> Model -> Model
          updateReactionsTo ers m = 
            Prelude.foldr 
             (\e m -> m & #profileReactionsTo % at (e ^. #eventId) ?~ (e, processContent e)) 
             m 
             (fst <$> ers)
      in 
        case mPm of 
          Nothing -> 
            effectSub model $ \sink -> 
               do 
                 liftIO . logError $ "Missing PagedNotes model for loading reactions"
                 sink NoAction
          Just pm ->
            effectSub (model & #profileReactions % at xo ?~ pm) $ \sink -> do 
              subscribe nn sink $
               allAtEOSOnPage 
                page
                [DatedFilter (EventsWithId (processed ^.. folded % _1 % #reactionTo)) Nothing Nothing]
                (\ers -> UpdateModel (updateReactionsTo ers) [])
              sink $ 
               LoadMoreIfNecessary (#profileReactions % ixAt xo) $
                 LoadMoreEvents (#profileReactions % at xo % non pm) page

    LoadMoreIfNecessary pml loadMoreAction -> 
      let 
        updAction =
          do
          pm <- model ^? pml
          let loadMore = 
               length (pm ^. #events) < (pm ^. #pgSize) * (pm ^. #pg) + (pm ^. #pgSize)
                    && (pm ^. #factor) < 100
          pure $ 
            if loadMore then 
              ((model & pml .~ (pm & #factor %~ (*2))), loadMoreAction)
            else 
              ((model & pml .~ (pm & #factor .~ 1)), NoAction)
      in 
        case updAction of 
          Just (updated, act) -> 
            updated <# pure act 
          _ -> 
            batchEff model [(liftIO . logError) "No model in PML!" >> pure NoAction]

    LoadProfile isLoadNotes isLoadFollowing xo page ->
      let 
         textNotes = defProfEvntsModel xo $ model ^. #now
         updated = model & #profileEvents % at xo ?~ textNotes
      in
         effectSub updated $ \sink -> do
          subscribe nn sink $
           periodicLoadProfileOnPage 
            page
            [DatedFilter (MetadataFilter [xo]) Nothing Nothing]
            ReceivedProfiles
          
          when isLoadNotes $ 
            sink $
             LoadMoreEvents (#profileEvents % at xo % non textNotes) page
          when isLoadFollowing $ 
            sink $ 
              LoadContactsOf 
                xo
                page
                (UpdateField (#profileContacts % at xo))
          sink $ LoadProfileReactions xo page

    SubState p sst ->
      let isError (Network.Error _) = True
          isError _ = False
          extract (Network.Error e) = Just e
          extract _ = Nothing
          -- timeouted relays, errored relays
          (toRels, erRels) =
            case sst of
              (_, SubFinished rs) ->
                -- find timeouted relays
                let trs = fst <$> filter ((== Running) . snd) (Map.toList rs)
                    ers = filter (isError . snd) (Map.toList rs)
                 in (trs, second extract <$> ers)
              _ -> ([], [])
          timeouts = (\r -> "Relay " <> (showt $ r ^. #uri) <> " timeouted") <$> toRels
          errors = (\(r, er) -> "Relay " <> (showt $ r ^. #uri) 
                                <> " returned error: " <> (fromMaybe "" er)) <$> erRels
      in effectSub model $ \sink -> do
            notify LB.loadingBar $ LB.UpdateSubscriptions p sst
            mapM_ sink $ Report ErrorReport <$> timeouts ++ errors

    DisplayProfilePage mid xo ->
      batchEff model [pure $ LoadProfile True True xo (ProfilePage xo), pure $ GoPage (ProfilePage xo) mid]

    LogConsole what ->
      model <# do
        liftIO (print what) >> pure NoAction

    UpdateModel updateF actions -> do 
      batchEff (updateF model) actions

    SendReplyTo e getReplyText -> do
      let replyEventF = \t -> do
            reply <- getReplyText
            pure $ createReplyEvent e t (model ^. #me) reply
          localhost = Relay "localhost" (RelayInfo False False) False
          successActs = [\se -> RepliesRecvNoEmbedLoading [(se, localhost)], 
              const (UpdateModel clearReplyInModel [clearReplyInStorage]),
              const (Report SuccessReport $ "Reply sent!")]
      effectSub model $ 
       signAndSend 
        replyEventF 
        successActs
        (singleton . const . Report ErrorReport $ "Failed sending reply!")
      where 
        clearReplyInModel :: Model -> Model
        clearReplyInModel m = m & #writeReplyTo .~ Nothing & #replyDraft .~ ""
        clearReplyInStorage = 
          do 
            setLocalStorage @T.Text "reply-draft" ""
            pure NoAction

    SendPost getPostText -> do 
      let createPost = 
           \t -> do 
            content <- getPostText
            pure $ textNote content (model ^. #me) t
          successActs = [const $ UpdateModel clearPostInModel [clearPostInStorage], 
                         const (Report SuccessReport $ "Post sent!"),
                         DisplayThread]
      effectSub model $ 
        signAndSend 
          createPost 
          successActs
          (singleton . const . Report ErrorReport $ "Failed sending post!")
     where 
      clearPostInModel :: Model -> Model
      clearPostInModel m = m & #postDraft .~ ""
      clearPostInStorage = 
        do 
          setLocalStorage @T.Text "post-draft" ""
          pure NoAction
        
    CreateInitialProfile -> do
        let me = model ^. #me
            p = def 
             {username="Fresh Dingo",
              picture=
                Just $ 
                "https://howtodrawforkids.com/wp-content/\
                \uploads/2022/04/how-to-draw-a-cute-frog.jpg"}
            newProfileF = \now -> do 
              pure $ setMetadata p me now
        effectSub model $ 
         signAndSend 
          newProfileF 
          []
          []

    SendUpdateProfile getProfile -> do
        let me = model ^. #me
        let newProfileF = \now -> do 
              p <- getProfile
              pure $ setMetadata p me now
        effectSub model $ 
         signAndSend 
          newProfileF 
          (singleton . const . LoadProfile False False me $ MyProfilePage) 
          (singleton . const . Report ErrorReport $ "Failed updating profile!")
        -- TODO: update profile in #profiles if sending successfull
    SendLike e -> do
        let me = model ^. #me
            rcs = model ^. #reactions % #processed % at (e ^. #eventId)
            sendLike = pure . likeEvent e me
            likeReactions = fromMaybe (Set.empty) $ rcs ^? _Just % at Like % _Just 
            isLikedByMe = Set.member me . Set.fromList $ likeReactions ^.. folded % #author
            doNothing = noEff model

        if isLikedByMe 
        then doNothing
        else 
          effectSub model $ 
           signAndSend 
            sendLike 
            (singleton . const . LikeSent $ e) 
            (singleton . const . Report ErrorReport $ "Failed sending like!")
       
    LikeSent e -> 
      let me = model ^. #me
          likeReaction = likeReactionOf me
          updated = model & #reactions % #processed % at (e ^. #eventId) 
             %~ \rcs -> 
              Just $ 
                case rcs of 
                  Nothing -> Map.fromList [(Like, Set.singleton likeReaction)]
                  Just ss -> ss & at Like %~ Just .
                      fromMaybe (Set.singleton likeReaction) . fmap (Set.insert likeReaction)
      in 
        noEff $ updated
    
    DisplayMyProfilePage -> 
        batchEff model $
          [ pure $ LoadProfile False False (model ^. #me) MyProfilePage,
            pure $ GoPage MyProfilePage Nothing
          ]
    
    DisplayWritePostPage ->
      effectSub model $ \sink -> do 
        draft <- fromRight "" <$> getLocalStorage "post-draft" 
        sink $ UpdateField (#postDraft) draft 
        sink $ GoPage WritePostPage Nothing
    
    WriteTextToStorage tid t -> 
      model <# do 
        setLocalStorage tid t
        pure NoAction

    DisplayThreadWithId eid -> 
      effectSub model $ \sink -> do 
       sink $ UpdateField (#findEventModel % #error) (Just "") 
       subscribe nn sink $ 
           SubscriptionParams
            { subType = AllAtEOS,
              subFilter = [anytimeF . EventsWithId $ [eid]],
              extractResults = getEventRelayEither,
              actOnResults = displayThread,
              actOnSubState = Nothing, --TODO
              cancelButton = Nothing,
              timeoutPerRelay = Nothing,
              reportError = reportErrorAction
            }
      where 
        displayThread :: [(Event, Relay)] -> Action
        displayThread (er:_) = DisplayThread $ fst er
        displayThread [] = UpdateField (#findEventModel % #error) (Just "Event not found on any connected relay!")

    _ -> noEff model

  where
    signAndSend makeEvent successActs failureActs sink = 
       do
          now <- liftIO getCurrentTime
          key <- liftIO $ getSecKey xo
          e <- makeEvent now
          let Keys _ xo _ = keys $ nn
              signed = signEvent e key xo
          maybe
            (sink $ Report ErrorReport "Failed sending: Event signing failed")
            ( \se -> do
                isSuccess <- liftIO . runInNostr $ sendAndWait se (Seconds 1)
                sequence_ . fmap (\f -> sink (f se)) 
                  $ bool 
                     failureActs 
                     successActs
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

    removeDeletetedAndDuplicates :: [(Event, Relay)] -> [Event]
    removeDeletetedAndDuplicates rs =
      let (deleteEvts, otherEvts) =
            Prelude.partition
              (\e -> e ^. #kind == Delete)
              (Set.toList . Set.fromList $ fst <$> rs) -- to eliminate duplicates
          deletions =
            catMaybes $
              deleteEvts
                & fmap
                  ( \e -> do
                      ETag eid _ _ <- getSingleETag e
                      pure (e ^. #pubKey, eid)
                  )
          evts =
            filter
              ( \e ->
                  not $
                    (e ^. #pubKey, e ^. #eventId)
                      `elem` deletions
              )
              otherEvts
       in evts

    -- Note: this only works correctly when subscription is AtEOS,
    --       i.e. all events are returned at once, not periodically as they arrive
    --       This allows to handle Delete events effectivelly.
    processReceivedEvents :: [(Event, Relay)] -> ([(Event, [Content])], [EventId], [XOnlyPubKey])
    processReceivedEvents rs =
      let evts = removeDeletetedAndDuplicates rs
          ecs = (\e -> (e, processContent e)) <$> evts
          embedded = filterBech . concat $ snd <$> ecs
          (eprofs, enotes) = partitionBechs embedded
       in (ecs, eprofs, enotes)

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

    runInNostr = runNostr nn

    waitForReconnect sink = 
      do 
        unconnected <- liftIO . runNostr nn $ RP.waitForActiveConnections (Seconds 10)
        case (length unconnected > 0) of 
          True ->
            sink . Report ErrorReport $ 
              "Unable to connect to these relays: " 
              <> showt (unconnected ^.. folded % #uri)
          False ->
            sink . Report SuccessReport $ 
              "Reconnected to all relays" 

-- subscriptions below are parametrized by Page. The reason is
-- so that one can within that page track the state (Running, EOS)
-- of those subscriptions
subscribeForWholeThread :: NostrNetwork -> Event -> Page -> Sub Action
subscribeForWholeThread nn e page sink = do
  let eids = [(e ^. #eventId)]
      replyTo = maybe [] singleton $ findParentEventOf e 
  subscribe nn sink $ 
    periodicUntilEOSOnPage
      page
      [anytimeF $ LinkedEvents eids, anytimeF $ EventsWithId (eids ++ replyTo)]
      (ThreadEvents page)

subscribeForEventsReplies :: NostrNetwork -> [EventId] -> Page -> Sub Action
subscribeForEventsReplies _ [] _ _ = pure ()
subscribeForEventsReplies nn eids page sink =
  -- TODO: this subscribes for whole threads for all of those eids. What you need is a lighter query which only gets the replies
  --       Seems like there is no protocol support for only subscribe to Reply e tags. You always subscribe for both Reply and Root e tags.
  --  this makes queries which only want replies (and not root replies) to a single event possibly very inefficient
  subscribe nn sink $ 
     periodicUntilEOSOnPage
      page
      [anytimeF $ LinkedEvents eids]
      (ThreadEvents page)

writeModelToStorage :: Model -> JSM ()
writeModelToStorage m = pure ()

safeUpdateLocalContacts :: Set.Set XOnlyPubKey -> JSM ()
safeUpdateLocalContacts newCs = do
  oldCs <- Set.fromList <$> loadContactsFromStorage
  -- in case difference is more than one contact, refuse updating locally stored contacts
  -- it's expected that if you follow/unfollow more than one contact at a time something is borked
  if (Set.size $ oldCs `Set.difference` newCs) <= 1 
   then setLocalStorage "my-contacts" $ Set.toList newCs 
   else liftIO . print $ "Not updating local contacts!"

loadContactsFromStorage :: JSM [XOnlyPubKey]
loadContactsFromStorage = fromRight defaultContacts <$> getLocalStorage "my-contacts"

defaultContacts :: [XOnlyPubKey]
defaultContacts = someContacts

appView :: Model -> View Action
appView m =
  -- div_ [onLoad AllLoaded] $
  div_ [] $
    [ component LB.loadingBar,
      newNotesIndicator,
      div_
        -- [class_ "main-container", id_ "top-top"]
        [class_ "main-container"]
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
            [ text $
                "Display " <> showt howMany <> " new " <> bool "note" "notes" (howMany > 1)
            ]
        ]

displayFollowingView :: Model -> XOnlyPubKey -> View Action
displayFollowingView m followersOf =
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
        <$> maybe [] Set.toList (m ^. #profileContacts % at followersOf)
      where
        -- in case profile was not found on any relay display pubKey in about
        emptyP xo = Profile "" Nothing (encodeBechXo xo) Nothing Nothing

    displayProfile :: (XOnlyPubKey, Profile) -> View Action
    displayProfile (xo, p) =
      div_
        [class_ "profile", id_ $ getProfileElementId xo]
        [ 
          div_
            [class_ "pic-container"]
            [displayProfilePic (Just $ getProfileElementId xo) xo $ p ^. #picture],
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
    [displayPagedEvents False Notes m #feed FeedPage]

data PagedWhat = Notes | Notifications

displayPagedContent :: Bool -> Model -> Lens' Model (PagedEventsModel a) -> Page -> View Action -> View Action
displayPagedContent showIntervals m pml screen content = 
   div_
    []
    [ bool (div_ [] []) (div_ [] [text (pu <> " --- " <> ps)]) showIntervals,
      div_
        [ class_ "load-previous-container",
          bool
            (class_ "remove-element")
            (class_ "visible")
            (pg > 0)
        ]
        [ span_
            [ class_ "load-previous",
              onClick (ShowPrevious pml)
            ]
            [text "=<<"]
        ]
      ,content
      ,div_
        [class_ "load-next-container"]
        [span_ [class_ "load-next", onClick (ShowNext pml screen)] [text ">>="]]
    ]
   where
    f = m ^. pml
    pgSize = f ^. #pgSize
    pg = f ^. #pg
    -- notes = take (pageSize * page + pageSize) $ f ^. #events
    events = f ^. #getEvent <$> (take pgSize . drop (pg * pgSize) $ f ^. #events)
    (Until until) = f ^. #until
    since' = f ^. #pgStart % at pg
    since = fromMaybe "" (showt <$> since')
    ps = maybe since (showt . fromSeconds . O.view #created_at . fst ) $ Prelude.uncons events
    pu = showt $ maybe until (fromSeconds . O.view #created_at . snd ) $ Prelude.unsnoc events

displayPagedEvents :: Bool -> PagedWhat -> Model -> (Lens' Model PagedEvents) -> Page -> View Action
displayPagedEvents showIntervals pw m pml screen =
 displayPagedContent showIntervals m pml screen $
      div_ [] [
        (case pw of 
          Notes -> 
            div_
              [class_ "notes-container"]
              (displayPagedNote m pml <$> notes) -- TODO: ordering can be different
          Notifications ->
            div_ [] (displayPagedNotif m pml <$> notes))
        ,div_ [id_ "notes-container-bottom"] []
      ]
  where
    f = m ^. pml
    pgSize = f ^. #pgSize
    pg = f ^. #pg
    -- notes = take (pageSize * page + pageSize) $ f ^. #events
    notes = take pgSize . drop (pg * pgSize) $ f ^. #events

displayPagedReactions :: Model -> Lens' Model PagedReactions -> Page -> View Action
displayPagedReactions m pml screen = 
  displayPagedContent True m pml screen $ 
   div_ [] $
      displayReaction <$> reactions
  where 
    f = m ^. pml
    pgSize = f ^. #pgSize
    pg = f ^. #pg
    reactions = take pgSize . drop (pg * pgSize) $ f ^. #events
    emptyEvent = div_ [] [text "Could not find event..."]
    displayEvent eid = fromMaybe emptyEvent $ do 
       ec <- m ^. #profileReactionsTo % at eid 
       pure $ displayNote m ec
    displayReaction (re,r) = 
      div_ [class_ "reaction-and-event"] 
        [ div_ [class_ "reaction"] [text $ (r ^. #content)]
        , div_ [class_ "reaction-to"] [displayEvent (re ^. #reactionTo)]]

footerView :: Model -> View action
footerView Model {..} =
  div_
    [class_ "footer"]
    []

displayProfilePic :: Maybe ElementId -> XOnlyPubKey -> Maybe Picture -> View Action
displayProfilePic mid xo (Just pic) =
  div_ [onClick $ DisplayProfilePage mid xo] 
       [component_ (Component img)]
  where 
    img = 
      imgWithMouseActions  
          [ class_ "profile-pic", 
            class_ "hovered",
            prop "src" $ pic
          ]
          [ class_ "profile-pic",
            prop "src" $ pic
          ]
      
  -- imgKeyed_ (Key pic)
  --   [ class_ "profile-pic",
  --     prop "src" $ pic,
  --     onClick $ DisplayProfilePage mid xo
  --   ]
displayProfilePic mid xo _ = 
  div_
    [ class_ "profile-pic",
      onClick $ DisplayProfilePage mid xo 
    ]
    []

displayNoteContent :: Bool -> Model -> (Event, [Content]) -> View Action
displayNoteContent withEmbed m (e,content) =
  let displayContent (TextC textWords) =
        let pgraphs = uncons . filter (/= "") . T.splitOn "\n\n" $ T.unwords $ textWords
        in div_ [] $ 
            maybe []
              (\(first,rest) -> text first : fmap (\ptext -> p_ [] [text ptext]) rest)
              pgraphs
      displayContent (LinkC Image link) =
        div_
          []
          [ a_
              [href_ link, target_ "_blank"]
              [imgKeyed_ (Key link) [class_ "link-pic", prop "src" link]]
          ] 
      displayContent (LinkC Video link) =
        div_
          []
          [ a_
              [href_ link, target_ "_blank"]
              [video_ [class_ "link-video", prop @T.Text "controls" " "] [source_ [prop "src" link]]]
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
            text . fromMaybe "<Failed encoding nevent>" . encodeBechEvent $ eid
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
          [displayProfilePic (Just $ getNoteElementId e) xo $ p ^. #picture],
        div_
          [class_ "info-container"]
          [ div_ [class_ "name"] [text $ p ^. #username],
            div_ [class_ "about"] [text . fromMaybe "" $ p ^. #about]
          ]
      ]

displayEmbeddedNote :: Model -> (Event, [Content]) -> View Action
displayEmbeddedNote m ec = displayNote' False m ec

displayNoteShort :: Bool -> Model -> (Event, [Content]) -> View Action
displayNoteShort withEmbed m ec@(e, _) =
  div_
    [class_ "text-note", onClick . LogConsole $ show e]
    [ displayNoteContent withEmbed m ec,
      div_
        [class_ "text-note-properties"]
        ( [showThreadIcon]
            ++ [displayReactions m e reactions]
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
    showThreadIcon =
      let count = maybe "" showt replies
      in div_
            [class_ "replies-count", onClick $ DisplayThread e]
            [ bool
                (text $ "▶ " <> count)
                (text $ "▽ " <> count)
                isThreadOf
            ]

    replyIcon = div_ [class_ "reply-icon", onClick $ DisplayReplyThread e] [text "↪"]

displayPagedNote :: Model -> (Lens' Model PagedEvents) -> (Event, [Content]) -> View Action
displayPagedNote m pml ec@(e,_) 
    | isJust (findParentEventOf e) =
        let mp = m ^. pml % #parents % at (e ^. #eventId)
        in 
          div_ [class_ "parent-child-complex"] 
           [div_ [class_ "parent"] [maybe emptyParent (\p -> displayNote m p) mp]
           ,div_ [class_ "child"] [displayNote m ec]]
    | otherwise = 
        div_ [class_ "parent-child-complex"] [displayNote m ec]
  where 
    emptyParent = div_ [] [text "Loading parent event"]

displayPagedNotif :: Model -> (Lens' Model (PagedEventsModel a)) -> (Event, [Content]) -> View Action
displayPagedNotif m pml ec@(e,_) =
    case e ^. #kind of 
      TextNote -> 
        notif [tnInfo, displayNote m ec]
      Reaction -> 
        notif [reactInfo, displayNote m reactTo]
      _ -> div_ [] [] 
  where 
    notif = div_ [class_ "notification", id_ $ getNoteElementId e]
    replyTo = m ^. pml % #parents % at (e ^. #eventId)
    unknown = Profile "" Nothing Nothing Nothing Nothing
    profile = fromMaybe unknown $ getAuthorProfile m e
    onProfileClick = 
      onClick $ DisplayProfilePage (Just $ getNoteElementId e) 
                                   (e ^. #pubKey)
    profileName = span_ 
     [class_ "username", onProfileClick] 
     [text $ profile ^. #username]
    displayName = span_ 
     [class_ "displayname", onProfileClick] 
     [text . fromMaybe "" $ profile ^. #displayName]
    isReplyToU = maybe False (\(p,_) -> p ^. #pubKey == m ^. #me) replyTo
    notifType = bool
      ("mentioned you in")
      ("replied")
      isReplyToU
    tnInfo = div_ [] [profileName, displayName, span_ [] [text $ " " <> notifType]]
    reactInfo = div_ [] [profileName, displayName, span_ [] [text $ " reacted with " <> decodeContent (e ^. #content) <> " to"]]
    decodeContent c = if c == "+" then "👍" else c 
    reactTo = fromMaybe (emptyEvent, processContent emptyEvent) $ do 
       eid <- (Nostr.Reaction.extract e) ^? _Just % #reactionTo
       m ^. pml % #reactionEvents % at eid
    emptyEvent = newEvent "<Could not find the event this reaction belongs to>" (m ^. #me) (m ^. #now) 

displayNote :: Model -> (Event, [Content]) -> View Action
displayNote = displayNote' True 

getProfileElementId :: XOnlyPubKey -> ElementId
getProfileElementId = T.pack . take 10 . exportXOnlyPubKey 

getNoteElementId :: Event -> ElementId
getNoteElementId e = T.take 10 . eventIdToText . getEventId $ e ^. #eventId

displayNote' :: Bool -> Model -> (Event, [Content]) -> View Action
displayNote' withEmbed m ec@(e, _) =
  div_
    [ class_ "note-container",
      id_ $ getNoteElementId e
    ]
    [ div_
        [class_ "profile-pic-container"]
        [displayProfilePic (Just $ getNoteElementId e) (e ^. #pubKey) $ picUrl m e],
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
        [class_ "displayname"]
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
      Following xo -> displayFollowingView m xo
      ThreadPage e -> displayThread m e
      ProfilePage xo -> displayProfile True m xo
      FindProfilePage -> displayFindProfilePage m
      FindEventPage -> displayFindEventPage m
      RelaysPage -> displayRelaysPage m
      MyProfilePage -> displayMyProfilePage m
      NotificationsPage -> displayNotificationsPage m
      WritePostPage -> displayWritePostPage m

rightPanel :: Model -> View Action
rightPanel m = div_ [class_ "right-panel"] [ul_ [] reports]
  where
    reports =
      ( \(i, reportType, report) ->
          liKeyed_
            (Key . showt $ i) -- so that Miso diff algoritm displays it in the correct order
            [ class_ $ bool "error" "success" (reportType == SuccessReport),
              class_ "hide-after-period"
            ]
            [text report]
      )
        <$> m ^. #reports

leftPanel :: Model -> View Action
leftPanel m =
  div_
    [class_ "left-panel"]
    [ div_
        []
        [ pItem "Following Feed" FeedPage,
          notifications,
          -- pItem "Followers"
          pItem "Following" $ Following (m ^. #me),
          pItem "Find Profile" FindProfilePage,
          pItem "Find Event" FindEventPage,
          pItem "Relays" RelaysPage,
          aItem "My Profile" DisplayMyProfilePage,
          aItem "Write Post" DisplayWritePostPage
          -- pItem "Bookmarks"
        ],
      div_ [class_ "logged-in-profile"] $
        [ div_ [] [text "logged in as: "],
          div_
            [ class_ "my-npub",
              onClick $ DisplayProfilePage Nothing (m ^. #me)
            ]
            [ text
                . (<> "...")
                . T.take 20
                . fromMaybe ""
                . encodeBechXo
                $ m ^. #me
            ]
        ]
          ++ (maybe [] 
                (\un -> [div_ [] [text . showt $ un]])
                (m ^? #profiles % at (m ^. #me) % _Just % _1 % #username)
             ),
      div_
        [bool (class_ "invisible") (class_ "visible") showBack, onClick (GoBack), class_ "back-button"]
        [backButton]
    ]
  where
    pItem label page =
      div_
        [class_ "left-panel-item"]
        [div_ [onClick (GoPage page Nothing)] [text label]]
    aItem label action =
      div_
        [class_ "left-panel-item"]
        [div_ [onClick action] [text label]]
    showBack = (> 1) . length $ m ^. #history
    backButton = img_ [id_ "left-arrow", prop "src" $ ("arrow-left.svg" :: T.Text)]
    notifications = 
      div_
       [class_ "left-panel-item"]
       [div_ [onClick ShowNotifications] 
             [text "Notifications", span_ [class_ "new-notifs-count"] [text newNotifsCount]]] 
    newNotifs = m ^. #notifsNew
    hasNewNotifs = length newNotifs > 0
    newNotifsCount = bool "" (" (" <> (showt $ length newNotifs) <> ")") hasNewNotifs

displayProfile :: Bool -> Model -> XOnlyPubKey -> View Action
displayProfile isShowNotes m xo =
  div_
    [class_ "profile-page"]
    [fromMaybe notFound profileDisplay]
  where
    displayProfileTabs :: Model -> View Action
    displayProfileTabs m =
      div_
        []
        [ div_
            [class_ "profile-tabs"]
            [ span_ [onClick setPostsTab, class_ postsTabStyle] [text "Posts"],
              span_ [onClick setReactionsTab, class_ reactionsTabStyle] [text "Reactions"]
            ],
          div_
            []
            [ case m ^. #profileTab of
                ProfilePosts -> notesDisplay
                ProfileReactions -> reactionsDisplay
            ]
        ]
    setPostsTab = UpdateModel (O.set #profileTab ProfilePosts) []
    setReactionsTab = UpdateModel (O.set #profileTab ProfileReactions) []
    postsTabStyle = bool "tab" "tab-selected" (m ^. #profileTab == ProfilePosts)
    reactionsTabStyle = bool "tab" "tab-selected" (m ^. #profileTab == ProfileReactions)
    notFound =
      div_
        []
        [ text
            "Looking for profile or profile not found on any of your relays..."
        ]
    banner p =
      p ^. #banner >>= \b ->
        pure $
          imgKeyed_ (Key b) [class_ "banner-pic", prop "src" b]
    bannerDef = div_ [class_ "banner-pic-default"] []
    profilepic p =
      p ^. #picture >>= \pic ->
        pure $
          imgKeyed_ (Key pic) [class_ "profile-pic", prop "src" pic]
    profilepicDef = div_ [class_ "profile-pic-default"] []
    profileName p = span_ [class_ "username"] [text $ p ^. #username]
    displayName p =
      span_
        [class_ "displayname"]
        [text . fromMaybe "" $ p ^. #displayName]
    rInfoText r = case (r ^. #info) of
      RelayInfo True True -> "(RW)"
      RelayInfo True False -> "(R)"
      RelayInfo False True -> "(W)"
      RelayInfo False False -> ""
    relays =
      m ^. #profileRelays % at xo >>= \(rels, _) ->
        pure $
          div_ [] . concat $
            ( ( \r ->
                  [ span_ [class_ "relay"] [text (r ^. #uri)],
                    span_
                      [class_ "relay-info"]
                      [text $ rInfoText r]
                  ]
              )
                <$> rels
            )
    relaysDisplay = div_ [class_ "profile-relays"] (maybe [] singleton relays)
    npub = div_ [class_ "npub"] [text . fromMaybe "" $ encodeBechXo xo]
    profileEvents = #profileEvents % at xo % non (defProfEvntsModel xo $ m ^. #now)
    notesDisplay =
      if (not isShowNotes)
        then (div_ [] [])
        else displayPagedEvents True Notes m profileEvents (ProfilePage xo)
    reactionsDisplay = displayPagedReactions m (#profileReactions % at xo % non (defProfReactionsModel xo (m ^. #now))) (ProfilePage xo)
    following = maybe [] Set.toList $ m ^. #profileContacts % at xo
    follows =
      div_
        [class_ "profile-follows", onClick $ DisplayProfileContacts xo (Following xo)]
        [text $ "follows " <> showt (length following) <> " profiles"]
    myContacts = #profileContacts % at (m ^. #me)
    profileDisplay = do
      (p, _) <- m ^. #profiles % at xo
      pure $
        div_
          []
          [ div_
              [class_ "banner"]
              [fromMaybe bannerDef (banner p)],
            div_
              [class_ "profile-pic-container"]
              [ fromMaybe profilepicDef (profilepic p),
                div_ [class_ "names"] [(profileName p), (displayName p), relaysDisplay, npub, follows],
                div_
                  [class_ "follow-button-container"]
                  [ if isJust $ m ^? myContacts % _Just % at xo % _Just
                      then
                        div_
                          []
                          [ span_ [class_ "follow-button"] [text "Following"],
                            button_ [class_ "unfollow-button", onClick (Unfollow xo)] [text "Unfollow"]
                          ]
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
            displayProfileTabs m
          ]

displayThread :: Model -> Event -> View Action
displayThread m e =
  let reid = RootEid $ fromMaybe (e ^. #eventId) $ findRootEid e
      isWriteReply = Just e == m ^. #writeReplyTo
      parentNotFound = div_ [class_ "parent-not-found"] 
        [text "Loading parent or parent not found on connected relays..."]
      parentDisplay = maybe (bool Nothing (Just parentNotFound) $ isReply e) Just $ do
        thread <- m ^. #threads % at reid
        parentId <- thread ^. #parents % at (e ^. #eventId)
        parent <- thread ^. #events % at parentId
        pure $ div_ [class_ "parent"] [displayNote m parent]

      repliesDisplay = do
        thread <- m ^. #threads % at reid
        let replies = getRepliesFor thread (e ^. #eventId)
        pure $ (\r -> (div_ [class_ "reply"] [displayNote m r])) <$> orderByAgeAsc replies

      replyInputEl = "reply-text-area"
      getReply = getValueOfInput replyInputEl
      writeReplyDisplay = 
        flip (bool Nothing) isWriteReply $ pure $
          div_
            [class_ "reply-box"]
            [ textarea_
                [ id_ replyInputEl,
                  defaultValue_ $ m ^. #replyDraft,
                  onChange $ WriteTextToStorage "reply-draft"
                ]
                [],
              button_ [onClick $ SendReplyTo e getReply] [text "Send"]
            ]
      noteDisplay =
        Just $
          div_
            [ class_ $
                if isJust parentDisplay
                  then "note"
                  else "note-no-parent"
            ] $
             [displayNote m (e, processContent e)]
             ++ maybe [] singleton writeReplyDisplay
             ++ fromMaybe [] repliesDisplay
   in div_ [class_ "thread-container"] $
        catMaybes [parentDisplay, noteDisplay]

displayReactions :: Model -> Event -> Maybe (Map.Map Sentiment (Set.Set Reaction)) -> View Action
-- displayReactions Nothing = div_ [class_ "reactions-container"] [text ("")]
displayReactions m e rcs =
  let howMany = showt . length
      likeReactions = fromMaybe (Set.empty) $ rcs ^? _Just % at Like % _Just 
      isLikedByMe = Set.member (m ^. #me) . Set.fromList $ likeReactions ^.. folded % #author
      likeCls = bool "like-reaction" "like-reaction-liked" $ isLikedByMe
      likeCnt = fromMaybe "" $ howMany <$> rcs ^? _Just % at Like % _Just
      likes = [span_ [class_ likeCls, onClick $ SendLike e] [text "♥"], span_ [] [text $ " " <> likeCnt]]
      dislikeCnt = fromMaybe "" $ howMany <$> rcs ^? _Just % at Dislike % _Just
      dislikes = span_ [class_ "dislike-reaction"] [text $ "🖓 " <> dislikeCnt]
      otherCnt = fromMaybe "" $ howMany <$> rcs ^? _Just % at Nostr.Reaction.Other % _Just
      others = span_ [class_ "other-reaction"] [text $ "Others: " <> otherCnt]
   in div_
        [class_ "reactions-container"]
        $ likes ++ [dislikes, others] 

displayFindEventPage :: Model -> View Action
displayFindEventPage m =
  div_ [class_ "find-profile"] $
    [div_ [class_ "search-box"] [search, searchButton]]
      ++ maybe [] (\err -> [div_ [class_ "error-msg"] [text err]]) error
  where
    bechEvent = m ^. #findEventModel % #bechEvent
    error = m ^. #findEventModel % #error
    searchAction = 
      case decodeBechEvent bechEvent of 
        Just eventId -> DisplayThreadWithId eventId
        Nothing -> UpdateField (#findEventModel % #error) (Just "Invalid bech event format")
    searchButton =
      button_
        [class_ "search-box-button", onClick searchAction]
        [text "Find"]
    search =
      input_
        [ class_ "input-xo",
          value_ bechEvent,
          placeholder_ "Enter event bech (nevent1, note1)",
          type_ "text",
          onInput $ UpdateField (#findEventModel % #bechEvent)
        ]

displayFindProfilePage :: Model -> View Action
displayFindProfilePage m =
  let npub = m ^. #findWho
      isNotBlank = not . T.null $ npub
      mxo = decodeNpub npub
      searchAction = maybe NoAction (\xo -> DisplayProfilePage Nothing xo) mxo
      search =
        input_
          [ class_ "input-xo",
            placeholder_ "Enter npub",
            value_ npub,
            type_ "text",
            onInput $ UpdateField #findWho
          ]
      searchButton =
        button_
          [class_ "search-box-button", onClick searchAction]
          [text "Find"]
   in div_ [class_ "find-profile"] $
        [div_ [class_ "search-box"] [search, searchButton]] 
         ++ bool [] [div_ [class_ "error-msg"] [text "Invalid NPub"]] ((not . isJust $ mxo) && isNotBlank)

displayNotificationsPage :: Model -> View Action
displayNotificationsPage m = 
  div_
    [class_ "notifications"]
    [displayPagedEvents True Notifications m #notifs NotificationsPage]

displayMyProfilePage :: Model -> View Action
displayMyProfilePage m =
  div_
    [class_ "myprofile-edit"]
    [ 
      --  inputKeyed_ (Key username)
      input_
        [ id_ iUsername,
          defaultValue_ username,
          placeholder_ "Enter Username",
          type_ "text"
        ], 
      -- inputKeyed_ (Key displayname) 
      input_
        [ id_ iDisplayname,
          -- onCreated $ SetInitialValue "myprofile.displayname" displayname,
          defaultValue_ displayname,
          placeholder_ "Enter Displayname",
          type_ "text"
        ],
      -- inputKeyed_ (Key about)
      input_
        [ id_ iAbout,
          defaultValue_ about,
          placeholder_ "Enter About",
          type_ "text"
        ], 
      -- inputKeyed_ (Key picture)
      input_
        [ id_ iPicture,
          defaultValue_ picture,
          placeholder_ "Enter profile pic URL",
          type_ "text"
        ],
      -- inputKeyed_ (Key banner)
      input_
        [ id_ iBanner,
          defaultValue_ banner,
          placeholder_ "Enter banner pic URL",
          type_ "text"
        ],
      button_
          [class_ "update-profile-button", onClick $ SendUpdateProfile getProfile]
          [text "Update"],
      displayProfile False m (m ^. #me)
    ]
  where 
    me = fromMaybe def $ fst <$> m ^. #profiles % at (m ^. #me)
    username = me ^. #username
    displayname = fromMaybe "" $ me ^. #displayName
    about = fromMaybe "" $ me ^. #about
    picture = fromMaybe "" $ me ^. #picture
    banner = fromMaybe "" $ me ^. #banner
    iUsername = "myprofile.username"
    iDisplayname = "myprofile.displayname"
    iAbout = "myprofile.about"
    iPicture = "myprofile.picture"
    iBanner = "myprofile.banner"
    getProfile =
      let get tid = getValueOfInput tid
          getM tid = do
            v <- getValueOfInput tid
            pure $ case v of
              "" -> Nothing
              _ -> Just v
       in Profile
            <$> get iUsername
            <*> getM iDisplayname
            <*> getM iAbout
            <*> getM iPicture
            <*> getM iBanner


displayRelaysPage :: Model -> View Action
displayRelaysPage m =
  div_ [class_ "relays-page"] $
    [info, relaysGrid, reloadInfo, inputRelay]
  
  where
    info = div_ [class_ "relay-info"] [text $ "Remove relays which time out to improve loading speed"]
    
    reloadInfo =
      div_
        [class_ "relay_info"]
        [button_ [onClick Reload] [text "Reload"], span_ [] [text " for changes to take effect"]]

    displayRelay (r, isActive, (isConnected, ErrorCount errCnt, CloseCount closeCnt)) =
      [ div_ [class_ ("relay-" <> bool "inactive" "active" isActive)] [text r],
        div_ [class_ ("relay-" <> bool "disconnected" "connected" isConnected)] [text $ bool "No" "Yes" isConnected],
        div_ [class_ "relay-error-count"] [text . showt $ errCnt],
        div_ [class_ "relay-close-count"] [text . showt $ closeCnt],
        div_ [class_ "relay-action"] 
         [div_ [onClick $ bool (ChangeRelayActive r True) (ChangeRelayActive r False) isActive] 
               [text (bool "Activate" "Deactivate" isActive)]],
        div_ [class_ "relay-action"]
         [div_ [class_ (bool "visible" "invisible" isActive), onClick (RemoveRelay r)] [text "Remove"] ]
      ]
    
    getStats r = 
      fromMaybe (False, ErrorCount 0, CloseCount 0) $
       m ^. #relaysStats % at (r ^. #uri)

    relaysGrid =
      div_ [class_ "relays-grid"] $
        gridHeader
          ++ displayRelays 
    (connected, notConnected) = 
      Prelude.partition (\sr -> fromMaybe False $ m ^? #relaysStats % at (sr ^. #relay % #uri) % _Just % _1) $ 
        m ^. #relaysList
    relaysStats = (\sr -> (sr ^. #relay % #uri, sr ^. #active, getStats (sr ^. #relay))) <$> 
       connected ++ notConnected
    displayRelays = concat (displayRelay <$> relaysStats)
    
    gridHeader =
      [ div_ [] [text "Relay"],
        div_ [] [text "Connected"],
        div_ [] [text "Errors count"],
        div_ [] [text "Close count"],
        div_ [] [], -- activate/deactivate
        div_ [] [] -- remove
      ]
    inputRelay =
      let relayInputEl = "relay-text-input"
          getNewRelayUrl = getValueOfInput relayInputEl
      in
      div_ [] [
        input_
          [ id_ relayInputEl,
            class_ "input-relay",
            type_ "text"
          ], 
        button_ [onClick $ AddRelay getNewRelayUrl] [text "Add relay"]]

displayWritePostPage :: Model -> View Action
displayWritePostPage m = 
    div_
      [class_ "new-post-page"]
      [ textarea_ 
          [ id_ "new-post-text-area",
            defaultValue_ $ m ^. #postDraft,
            onChange $ WriteTextToStorage "post-draft"
          ]
          [],
        button_ [onClick $ SendPost getPost] [text "Send"]
      ]
    where
      getPost = getValueOfInput "new-post-text-area"

getAuthorProfile :: Model -> Event -> Maybe Profile
getAuthorProfile m e = fst <$> m ^. #profiles % at (e ^. #pubKey)

picUrl :: Model -> Event -> Maybe MisoString
picUrl m e = do
  Profile {..} <- getAuthorProfile m e
  picture

loadKeys :: JSM (Keys, Bool)
loadKeys = do
  let identifier = "my-keys"
  keys <- getLocalStorage identifier
  case keys of
    Right k -> pure (k, False)
    Left _ -> do
      newKeys <- liftIO $ generateKeys
      setLocalStorage identifier newKeys
      pure (newKeys, True)

eventAge :: UTCTime -> Event -> String
eventAge now e =
  let ageSeconds =
        round $ diffUTCTime now (fromSeconds $ e ^. #created_at)
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

