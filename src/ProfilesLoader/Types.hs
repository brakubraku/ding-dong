module ProfilesLoader.Types where 
import MyCrypto
import Nostr.Profile
import Data.Time
import Nostr.Relay

type ProfOrRelays = 
     (XOnlyPubKey, 
      Maybe (Profile, UTCTime, Relay), 
      Maybe ([Relay], UTCTime))


