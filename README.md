
# DingDong - Nostr web client in Haskell

Try it here (https://brakubraku.github.io/dingydongy/). I have ever only run it on Firefox, so have no idea how it behaves in other browsers.

Seeing that badass full Haskell web development has been enabled by GHC WASM backend and looking for a project to flex my Haskell muscle on, I came across the idea of social media based on simple exchange of messages, signed by account's private key - Nostr. 

"Privatized" social media suffer many ailments:
* You don't decide how your feed looks like...
* You don't own your identity, because it can be arbitarily banned...
* You are unable to confront those who "block" you...
* You can be shadow banned...
* ... 

These problems either don't exist or can be solved using Nostr protocol.

"Privatized" social media does not create/have features which are designed to primarily bring value to you.
* I want to be notified when accounts from some set I created, comment on some threads I am interested in...
* I want to find other accounts, by how many endorsments (positive reactions?) they received from a set of other accounts I like...
* I want to be able to automatically mute accounts, which are muted by some accounts I trust...
* I want to be able to do my own analytics...
* ...

With social media implemented on an open/decentralized protocol, you can implement whatever feature you find traditional "socials" lack. 

That being said, by Nostr being open, you run into all kinds of content. The power to filter that however you want is in your hands.

Get inspired by more mature Nostr clients out there, and bring those features here!

# General
* The crypto part has not gone through any review, let alone audit. It's fine for playing, but just to be safe, assume the generated keys to be weak and leak easily.
* Your secret keys are sitting in browser's local storage unencrypted. For now. Once you wipe those, you will no longer be able to post with that identity again. Gone. If somebody else takes a hold of them, they will be able to post under this identity.
* Parts of this project (mostly data records and their *JSON instances) are taken from [prolic](https://github.com/prolic/)'s Futr project.
* Contribute! If you have a question or an idea for a feature, open an issue on github. If you plan to work on some feature, announce it first, so the work is not duplicated.
* Many thanks to [terrorJack](https://github.com/TerrorJack/) and [amesgen](https://github.com/amesgen) and the rest of #haskell-wasm and #haskell channels for advice/troubleshooting.
* I am happy to answer questions and advice on how to implement stuff...

# Current state

* None of the desirable features I touched upon in the introduction, are implemented in this client so far. At this point only the basic functionality is done and even that is not very robust.

* Click on stuff and see what happens, that's the userguide. 

* Your keys will be generated first time you load it, and then stored in browser's local storage. Change your profile info in "My Profile" section.

* I have made some default "following", by choosing some popular accounts as well as some other accounts I run across by chance. That does not mean I automatically endorse any of their posts.

* Likewise I have chosen some default relays which were working okay for me, but it's likely those will not be the best for people from other geographic regions.

* At the moment, the feed is only displaying original posts and replies from the accounts you follow (i.e. you will not see content from accounts you are not following, unless any of the accounts you follow reply to such content).

# Building
* start nix shell with haskell WASM buildtools 
    * I am using this one (thanks [terrorJack](https://github.com/TerrorJack/))
         https://gitlab.haskell.org/ghc/ghc-wasm-meta/-/archive/wasm-th-tmp/ghc-wasm-meta-wasm-th-tmp.tar.gz 
   
    * fire it up by running 
     ```
     nix shell https://gitlab.haskell.org/ghc/ghc-wasm-meta/-/archive/wasm-th-tmp/ghc-wasm-meta-wasm-th-tmp.tar.gz      --extra-experimental-features nix-command --extra-experimental-features flakes
     ```
 All the next steps assume you are inside the above shell
* you need to build libsecp256k1 C library for WASM platform first
    * download or clone from here https://github.com/bitcoin-core/secp256k1/
    * configure and install  (change  `some-directory` to where you want to library to land after make install)
     ```
     ./autogen.sh
     ./configure --prefix=some_directory CC=wasm32-wasi-clang --host=wasm32-unknown-wasi --enable-module-schnorrsig  CPPFLAGS=-D__OpenBSD__ CFLAGS="$CONF_CC_OPTS_STAGE2"
     ./make
     ./make install
     ```
* open ding-dong.cabal and change `extra-lib-dirs:` to `some_directory` (or wherever you installed libsecp256k1 to)
* go to `frontend` directory and do 
    * ```./build.sh && pushd dist; NIXPKGS_ALLOW_INSECURE=1 nix-shell -p python --run 'python -m SimpleHTTPServer'; popd```
         * this will build and deploy to localhost:8000
* Firefox will cache "bin.wasm" so remember to reload with Ctrl+Shift+r (in firefox) to wipe the caches, otherwise it will load the old version of the app. Same goes for CSS files.
* Note: I have only tested it on Firefox

# Architecture
* Haskell compiled to WebAssembly, using Miso as a frontend library.
* To get a better idea about how this combo works, take a look at a more didactic example here https://github.com/tweag/ghc-wasm-miso-examples 
* Notable modules 
    * DingDong - Miso start/update/view functions
    * ModelAction - Miso model/submodels/actions
    * Nostr.WebSocket - since webassembly runtime does not have network access/implementation I rely on browser's WebSocket javascript interface to facilitate websocket communication.

* Take a look at commits which implement a particular feature, to get a better idea of how to contribute. Some examples: 
    * [sending likes](https://github.com/brakubraku/ding-dong/commit/f52fc912d231b25d5786e11709ad6d21ab2cde9e)
    * [notifications page](https://github.com/brakubraku/ding-dong/commit/09850e432a05a42fcdc90d8c74a7cd2f30b1b1d2#diff-701633487fc911655ab4f087bc7f7b7cde9ec1f29e76c6e4d49d19028870c6f9R231)