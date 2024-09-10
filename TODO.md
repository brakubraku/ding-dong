Sep 10
* subscription architecture refactor
* follow button on search by npub
* display images/videos/links inside note (<video> <img> tags plus max-width:30% to preserve aspect ratio)


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
* display quoted notes inside note
* more event reactions support
* feed algorithm
* groups of authors
* write a post inside of thread

* DONE 
    * unsubscribing after EOSed!
        * modify so that you can have subscriptions which don't automatically unsubscribe on  EOS but continue running - such as the feed thread

    * author profile page
    * display thread

* BUGS
    * diffing algorithm results in previous images being displayed while new one loads?
    * secp256k1 signatures incorrent on wasm
    * if you open a long thread, you can see it for some reason scrolls all the way down