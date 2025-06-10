options:
with (builtins.fromJSON (builtins.readFile ./nixpkgs.json));
let
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/alexfmpe/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  };
  config = {
    allowUnfree = true;
    allowBroken = false;
  };
  overlays = [ (import ./overlay.nix) ] ++ options.overlays;
  pkgs = import nixpkgs { inherit overlays config; };
in
{
  inherit pkgs;
}
