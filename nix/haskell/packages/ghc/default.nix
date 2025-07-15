pkgs:
let
  source = import ../../../source.nix pkgs;
in
with pkgs.haskell.lib;
self: super:
{
  /* miso */
  miso = self.callCabal2nix "miso" source.miso {};
  miso-lynx = self.callCabal2nix "miso-lynx" source.miso-lynx {};

  /* examples */
  miso-lynx-examples = self.callCabal2nix "miso-lynx-examples" source.examples {};
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
