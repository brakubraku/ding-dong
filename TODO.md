Sep 10
* subscription architecture refactor
    * load reactions and profiles in a way that state of their subscription can be tracked, i.e. not via PeriodicLoader
* more robust websocket handling - all gets fucked when relays "misbehave"

* display images/videos/links inside note (<video> <img> tags plus max-width:30% to preserve aspect ratio)
* display quoted notes inside note

* cache
* recent actions page
    * you unfollowed 
    * you followed
    * you published
    * you replied
    * you liked/...
* verify signatures on all incoming events!
* relay management
* go forward/go back smoothely and with correct scroll (HistoryAPI)
* more event reactions support
* feed algorithm
* groups of authors
* write a post inside of thread

* DONE 
    * see author's posts on the profile page
    * follow button on search by npub
    * unsubscribing after EOSed!
        * modify so that you can have subscriptions which don't automatically unsubscribe on  EOS but continue running - such as the feed thread

    * author profile page
    * display thread

* BUGS
    * diffing algorithm results in previous images being displayed while new one loads?
    * secp256k1 signatures incorrent on wasm
    * if you open a long thread, you can see it for some reason scrolls all the way down