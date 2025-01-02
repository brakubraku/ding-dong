{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Relay.Database where

import Control.Monad.State.Lazy
import qualified Data.Map.Strict as M
import Data.Maybe
import MyCrypto
import Nostr.Event
import Nostr.Kind
import Relay.Request
import Optics

import Data.List (intersect)
import Nostr.Keys 
import Data.Time
import Data.DateTime

data DB = DB
  { byAuthor :: M.Map XOnlyPubKey [Event],
    byId :: M.Map EventId [Event],
    byKind :: M.Map Kind [Event],
    byEtag :: M.Map EventId [Event],
    byPtag :: M.Map XOnlyPubKey [Event]
  }

runFilter :: DB -> Filter -> [Event]
runFilter DB {..} Filter {..} = intersection candidates
  where 
   candidates = catMaybes $
    [ kinds >>= \ks -> Just . concat . catMaybes $ (flip M.lookup byKind) <$> ks,
      etags >>= \ets -> Just . concat . catMaybes $ (flip M.lookup byEtag) <$> ets,
      authors >>= \as -> Just . concat . catMaybes $ (flip M.lookup byAuthor) <$> as,
      ptags >>= \pts -> Just . concat . catMaybes $ (flip M.lookup byPtag) <$> pts,
      ids >>= \ids -> Just . concat . catMaybes $ (flip M.lookup byId) <$> ids
    ]
   intersection [] = []
   intersection cs = Prelude.foldr1 intersect cs

runFilters :: DB -> [Filter] -> [Event]
runFilters db fs = concat $ runFilter db <$> fs

add :: Event -> DB -> DB 
add e DB{..} = 
    DB { 
        byAuthor = addTo (e ^. #pubKey) byAuthor,
        byId = addTo (e ^. #eventId) byId,
        byKind = addTo (e ^. #kind) byKind,
        byEtag = foldr addTo byEtag etags, 
        byPtag = foldr addTo byPtag ptags
     }
 where 
    addTo id m = m & at id %~ Just . maybe [e] (\es -> e : es)
   
    etags = catMaybes $ getEventId <$> getETags e 
    ptags = catMaybes $ getXOnlyPubKey <$> getPTags e

    getEventId (ETag eid _ _) = Just eid
    getEventId _ = Nothing
    
    getXOnlyPubKey (PTag xo _ _) = Just xo
    getXOnlyPubKey _ = Nothing
    
emptyDB :: DB
emptyDB = DB M.empty M.empty M.empty M.empty M.empty

buildTestDB :: [Event] -> DB 
buildTestDB es = Prelude.foldr add emptyDB es

createEvent :: UTCTime -> Keys -> Maybe Event
createEvent when Keys{..} = 
  let ue = UnsignedEvent {
      pubKey' = xo,
      created_at' = toSeconds when,
      kind' = TextNote,
      tags' = [],
      content' = "so what"
    }
  in signEvent ue secKey xo

someEvents :: IO [Event]
someEvents = do 
  now <- Data.Time.getCurrentTime
  newKeys <- generateKeys
  pure $ 
   catMaybes . take 10 . repeat $ createEvent now newKeys 