{ pkg ? "ghc" }:

with (import ./default.nix {});

if pkg == "ghcjs"
then miso-native.env
else miso-native-ghc9122.env
