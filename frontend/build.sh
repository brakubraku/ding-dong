#!/usr/bin/env bash

set -e

if [[ $PWD != */frontend ]]; then
    echo "This script is meant to be run in the frontend directory"
    exit 1
fi

rm -rf dist
mkdir dist
cp ./*.html dist/
cp ./*.css dist/
cp ./*.svg dist/
cp -r ./browser_wasi_shim dist/

pushd ../
if command -v wasm32-wasi-cabal &>/dev/null; then
    wasm32-wasi-cabal build --allow-newer
else
    cabal \
        build \
        --with-compiler=wasm32-wasi-ghc \
        --with-ghc-pkg=wasm32-wasi-ghc-pkg \
        --with-hsc2hs=wasm32-wasi-hsc2hs \
        --with-gcc=wasm32-wasi-clang \
        --with-ld=wasm-ld
fi
popd

hs_wasm_path=$(find .. -name "*.wasm")

"$(wasm32-wasi-ghc --print-libdir)"/post-link.mjs \
     --input "$hs_wasm_path" --output ghc_wasm_jsffi.js

if $dev_mode; then
    cp "$hs_wasm_path" dist/bin.wasm
else
    wizer --allow-wasi --wasm-bulk-memory true --init-func _initialize -o dist/bin.wasm "$hs_wasm_path"
    wasm-opt ${1+"$@"} dist/bin.wasm -o dist/bin.wasm
    wasm-tools strip -o dist/bin.wasm dist/bin.wasm
fi

cp ./*.js dist

