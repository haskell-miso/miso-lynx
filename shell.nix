{ pkg ? "ghc" }:

with (import ./default.nix {});

if pkg == "ghcjs"
then miso-lynx.env
else miso-lynx-ghc9122.env
