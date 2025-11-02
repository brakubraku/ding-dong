{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module TestEvents where

import Nostr.Event
import Nostr.Keys
import Nostr.Kind
import Data.Time
import Data.DateTime
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text as T
import Miso.String (ms)

-- Sample test events for testing purposes

-- Sample keys for testing - Alice
aliceKeys :: Keys
aliceKeys = unsafePerformIO generateKeys

-- Sample keys for testing - Bob
bobKeys :: Keys
bobKeys = unsafePerformIO generateKeys

-- Sample keys for testing - Charlie
charlieKeys :: Keys
charlieKeys = unsafePerformIO generateKeys

testTime :: UTCTime
testTime = UTCTime (Data.Time.fromGregorian 2024 1 1) 0

-- Wrapper function to sign events with Keys type
signWith :: Keys -> UnsignedEvent -> Event
signWith keys ue = case signEvent ue (secKey keys) (xo keys) of
  Just event -> event
  Nothing -> error "Failed to sign event"

-- Alice's text note event
aliceTextEvent :: Event
aliceTextEvent = signWith aliceKeys $ UnsignedEvent
  { created_at' = toSeconds testTime
  , kind' = TextNote
  , tags' = []
  , content' = "This is a sample text note for testing"
  }

-- Alice's metadata event
aliceMetadataEvent :: Event
aliceMetadataEvent = signWith aliceKeys $ UnsignedEvent
  { created_at' = toSeconds testTime
  , kind' = Metadata
  , tags' = [XTag "test"]
  , content' = "{\"name\":\"Alice Johnson\",\"about\":\"Software developer and test user\"}"
  }

-- Bob's reaction event (reacting to Alice's text)
bobReactionEvent :: Event  
bobReactionEvent = signWith bobKeys $ UnsignedEvent
    { created_at' = toSeconds testTime + 100
    , kind' = Reaction
    , tags' = [ ETag (eventId aliceTextEvent) Nothing (Just Mention)
              , PTag (xo aliceKeys) Nothing Nothing
              ]
    , content' = "+"
    }

-- Charlie's reply event (replying to Alice's text)
charlieReplyEvent :: Event
charlieReplyEvent = signWith charlieKeys $ UnsignedEvent
    { created_at' = toSeconds testTime + 200
    , kind' = TextNote
    , tags' = [ ETag (eventId aliceTextEvent) Nothing (Just Reply)
              , PTag (xo aliceKeys) Nothing Nothing
              ]
    , content' = "This is Charlie's reply to Alice's text event"
    }

simpleContacts :: Keys -> Event 
simpleContacts me = 
  signWith me $ UnsignedEvent
      { created_at' = toSeconds testTime + 200
      , kind' = Metadata
      , tags' = [ PTag (xo aliceKeys) Nothing Nothing ]
      , content' = "Contacts of" <> (ms . show) (xo me)
      }

-- List of all test events
allTestEvents :: [Event]
allTestEvents = 
  [ aliceTextEvent
  , aliceMetadataEvent
  , bobReactionEvent
  , charlieReplyEvent
  ]
