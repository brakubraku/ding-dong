{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.ghc-wasm.url = "gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs =
    {
      self,
      nixpkgs,
      ghc-wasm,
      flake-utils
    }: 
     flake-utils.lib.eachSystem
      [
        "x86_64-linux"
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
      ]
      (
        system: 
            let 
                pkgs = import nixpkgs { inherit system; };
                wasi-sdk = ghc-wasm.packages.${system}.wasi-sdk;
                libsecp256k1 = pkgs.callPackage ./nix/libsecp256k1.nix { inherit wasi-sdk; };
            in
            { devShells.default = pkgs.mkShell {
                name = "ding-dong-shell";

                buildInputs = [
                    ghc-wasm.packages.${system}.all_9_10
                    libsecp256k1.dev
                ];

                shellHook = ''export PKG_CONFIG_PATH=${libsecp256k1.dev}/lib/pkgconfig'';
                };
            }
      );
}