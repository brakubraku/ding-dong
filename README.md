
# Building
* start nix shell from ghc-wasm-miso-examples project
** NIXPKGS_ALLOW_INSECURE=1 nix-shell -p python --run 'python -m SimpleHTTPServer'

* go to frontend directory and do 
** ./build.sh && pushd dist; NIXPKGS_ALLOW_INSECURE=1 nix-shell -p python --run 'python -m SimpleHTTPServer'; popd
*** this will build it and start running a web server on localhost:8000