pkgs:
let
  source = import ../../../source.nix pkgs;
in
with pkgs.haskell.lib;
self: super:
{
  /* miso */
  miso = self.callCabal2nix "miso" source.miso {};
  miso-native = self.callCabal2nix "miso-native" source.miso-native {};

  /* deps */
  jsaddle = self.callCabal2nix "jsaddle" "${source.jsaddle}/jsaddle" {};
  ghcjs-base = self.callCabal2nix "ghcjs-base" source.ghcjs-base {};

  /* examples */
  miso-native-examples = self.callCabal2nix "miso-native-examples" source.examples {};

  /* cruft */
  crypton = dontCheck super.crypton;
  cryptonite = dontCheck super.cryptonite;
  monad-logger = doJailbreak super.monad-logger;
  string-interpolate = doJailbreak super.string-interpolate;
  servant-server = doJailbreak super.servant-server;

  /* correct emscripten options to target Quick/PrimJS */
  ghc = super.ghc.overrideAttrs (drv: drv // {
    patches = (drv.patches or []) ++ [ ../../../patches/jsbi.patch ];
  });

}

