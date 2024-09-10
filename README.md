# Value

* Running Nostr queries/publishing to Nostr with the convenience of Web UI 
    * rapid WYSIWYG screens development; no need for any REST middle layer... 

* Nostr social media client 



# Building
* start nix shell from ghc-wasm-miso-examples project
    * nix develop --extra-experimental-features nix-command --extra-experimental-features flakes --offline
* go to frontend directory and do 
    * ./build.sh && pushd dist; NIXPKGS_ALLOW_INSECURE=1 nix-shell -p python --run 'python -m SimpleHTTPServer'; popd
        * this will build it and start running a web server on localhost:8000

# Crypto 

generate sec key (32 bytes of randomness) 
-> (from sec key) generate keypair 
-> (from keypair) generate xonlypubkey

save sec key (to sign) and xonlypubkey (to identify yourself and for others to verify your signatures)

You can probably simplify further and only store sec key (you can derive xonlypub key from it)



