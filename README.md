
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

save keypair (to sign) and xonlypubkey (to identify yourself and for others to verify your signatures)