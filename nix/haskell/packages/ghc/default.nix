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

  /* examples */
  miso-native-examples = self.callCabal2nix "miso-native-examples" source.examples {};
  jsaddle = self.callCabal2nix "jsaddle" "${source.jsaddle}/jsaddle" {};
  jsaddle-warp =
    dontCheck (self.callCabal2nix "jsaddle-warp" "${source.jsaddle}/jsaddle-warp" {});

  /* cruft */
  crypton = dontCheck super.crypton;
  cryptonite = dontCheck super.cryptonite;
  monad-logger = doJailbreak super.monad-logger;
  string-interpolate = doJailbreak super.string-interpolate;
  servant-server = doJailbreak super.servant-server;
}
