{ callPackage
, makeWrapper
, stdenvNoCC
, fetchFromGitHub
, autoreconfHook
, breakpointHook
, pkg-config
, wasi-sdk
}:
 stdenvNoCC.mkDerivation {
  name = "libsecp256k1";

  src = fetchFromGitHub {
   owner = "bitcoin-core";
   repo = "secp256k1";
   rev = ''4c57c7a5a9531874e965379119621f1ab500f2fe'' ;
   sha256 = "0z3w7r794s9s1z9ps70kyxhrlzdmwxh3z76z6gai1lka88q0frxl";
   };

  outputs = [
    "out" 
    "dev"
  ];

  nativeBuildInputs = [
    makeWrapper
    wasi-sdk
    autoreconfHook
    pkg-config
    # breakpointHook
  ];

  configureFlags = [
    "--host=wasm32-wasi"
    "--enable-module-schnorrsig"
    "CPPFLAGS=-D__OpenBSD__"
    # ("SECP_CFLAGS=\"-fvisibility=default -fPIC " + (builtins.getEnv "CONF_CC_OPTS_STAGE2") + ''"'')
    ("SECP_CFLAGS=-fPIC " + (builtins.getEnv "CONF_CC_OPTS_STAGE2"))
    ];
    
  postInstall = ''
    wasm32-wasi-clang -shared -Wl,--whole-archive $out/lib/libsecp256k1.a -o $out/lib/libsecp256k1.so
  '';

 }
