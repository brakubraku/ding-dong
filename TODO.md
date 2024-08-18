* port nostr-client to secp256k1-haskell
* port nostr-client to websocket functionality from Miso JSM websocket

* looks like verifying signatures on received events does not work
** signature should equal eventId signed by pubKey
*** potential cause: deserialization from JSON to Event does not work correctly?
