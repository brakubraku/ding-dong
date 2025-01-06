{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Nostr.Reaction where

import Crypto.Secp256k1 (XOnlyPubKey)
import Data.Map as M
import Data.Set as S
import GHC.Generics
import Nostr.Event
import qualified Nostr.Kind as Kind
import Nostr.Relay
import Optics
import qualified Data.Text as T

data Sentiment = Like | Dislike | Other -- Agree | Disagree
  deriving (Show, Eq, Ord) -- TODO: add emojis and whatnot support

data ReactionEvent = ReactionEvent
  { reactionTo :: EventId,
    event :: Event
  }
  deriving (Generic, Eq, Ord)

data Reaction = Reaction
  { author :: XOnlyPubKey,
    sentiment :: Sentiment,
    content :: T.Text
  }
  deriving (Generic, Show, Eq)

extract :: Event -> Maybe ReactionEvent
extract event
  | (event ^. #kind) /= Kind.Reaction = Nothing
  | otherwise = do
      reactionTo <- reactionToEvent event
      pure ReactionEvent {..}

getReaction :: ReactionEvent -> Reaction
getReaction ReactionEvent {event} =
  let cnt = event ^. #content
      st = case cnt of
        "+" -> Like
        "" -> Like
        "-" -> Dislike
        _ -> Other
   in Reaction (event ^. #pubKey) st cnt

addReaction ::
  ReactionEvent ->
  Map Sentiment (Set XOnlyPubKey) ->
  Map Sentiment (Set XOnlyPubKey)
addReaction e rs =
  let r = getReaction e
   in rs
        & at (r ^. #sentiment)
        %~ \authors ->
          case authors of
            Nothing -> Just (S.singleton (r ^. #author))
            _ -> S.insert (r ^. #author) <$> authors

data Reactions = Reactions
  { received :: Map ReactionEvent (Set Relay),
    processed :: Map EventId (Map Sentiment (Set XOnlyPubKey))
  }
  deriving (Generic, Eq)

processReceived :: Reactions -> (ReactionEvent, Relay) -> Reactions
processReceived reactions (event, relay) =
  let updateProcessed e =
        #processed
          % at (e ^. #reactionTo)
          %~ ( \sm ->
                 case sm of
                   Nothing ->
                     addReaction e <$> Just M.empty
                   _ ->
                     addReaction e <$> sm
             )
   in reactions
        & #received
        %~ unionWith
          (\rs1 rs2 -> rs1 `S.union` rs2)
          (M.fromList [(event, S.singleton relay)])
        & updateProcessed event

reactionToEvent :: Event -> Maybe EventId
reactionToEvent e = do
  (ETag eid _ _) <- snd <$> (unsnoc . Prelude.filter isEtag . tags $ e)
  pure eid
