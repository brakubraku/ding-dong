
WIP

# Building
* start nix shell with haskell WASM buildtools 
    * I am using this one 
         https://gitlab.haskell.org/ghc/ghc-wasm-meta/-/archive/wasm-th-tmp/ghc-wasm-meta-wasm-th-tmp.tar.gz 
   
    * fire it up by running 
     ```
     nix shell https://gitlab.haskell.org/ghc/ghc-wasm-meta/-/archive/wasm-th-tmp/ghc-wasm-meta-wasm-th-tmp.tar.gz      --extra-experimental-features nix-command --extra-experimental-features flakes
     ```
* need to build libsecp256k1 C library for WASM platform first
    * download or clone from here https://github.com/bitcoin-core/secp256k1/
    * run 
     ```
     ./autogen.sh
     ./configure CC=wasm32-wasi-clang --host=wasm32-unknown-wasi --enable-module-schnorrsig  CPPFLAGS=-D__OpenBSD__ CFLAGS="$CONF_CC_OPTS_STAGE2"
     ./make
     ./make install
     ```
* go to `frontend` directory and do 
    * ```./build.sh && pushd dist; NIXPKGS_ALLOW_INSECURE=1 nix-shell -p python --run 'python -m SimpleHTTPServer'; popd```
         * this will build and deploy to localhost:8000

