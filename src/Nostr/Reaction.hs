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
  deriving (Generic, Show, Eq, Ord)

extract :: Event -> Maybe ReactionEvent
extract event
  | (event ^. #kind) /= Kind.Reaction = Nothing
  | otherwise = do
      reactionTo <- reactionToEvent event
      pure ReactionEvent {..}

getReaction :: ReactionEvent -> Reaction
getReaction ReactionEvent {event} =
  let ct = event ^. #content
      st = case ct of
        "+" -> Like
        "" -> Like
        "-" -> Dislike
        _ -> Other
   in Reaction (event ^. #pubKey) st (replace st ct)
  where 
    replace st ct = 
      case st of
        Like -> "👍"
        Dislike -> "👎"
        _    -> ct

addReaction ::
  ReactionEvent ->
  Map Sentiment (Set Reaction) ->
  Map Sentiment (Set Reaction)
addReaction e rs =
  let r = getReaction e
   in rs
        & at (r ^. #sentiment)
        %~ \recs ->
          case recs of
            Nothing -> Just (S.singleton r)
            _ -> S.insert r <$> recs

data Reactions = Reactions
  { received :: Map ReactionEvent (Set Relay),
    processed :: Map EventId (Map Sentiment (Set Reaction))
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

likeReactionOf :: XOnlyPubKey -> Reaction
likeReactionOf xo = Reaction xo Like "👍"