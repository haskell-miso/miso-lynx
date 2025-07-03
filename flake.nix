{
  description = "A flake for acquiring GHCJS 9.12.2 modded w/ the JSBI.patch";

  inputs = {

    nixpkgs.url =
      "https://github.com/alexfmpe/nixpkgs/archive/b594b289740a2bc917ed9c66fef5d905f389cb96.tar.gz";

    flake-utils.url =
      "github:numtide/flake-utils";

  };
  outputs = { self, nixpkgs, flake-utils }:

   flake-utils.lib.eachSystem
      [
        "x86_64-linux"
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
      ] (system:

      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ (import ./nix/overlay.nix) ];
          config = {
            allowUnfree = true;
            allowBroken = false;
          };
        };

      in {
        nixConfig = {
          extra-substituters = [
            "https://haskell-miso-cachix.cachix.org"
          ];
          extra-trusted-public-keys = [
            "haskell-miso-cachix.cachix.org-1:m8hN1cvFMJtYib4tj+06xkKt5ABMSGfe8W7s40x1kQ0="
          ];
        };
        devShells.default =
          pkgs.mkShell {
            name = "miso-lynx shell";
            buildInputs = with pkgs; [
              pkgs.pkgsCross.ghcjs.haskell.packages.ghcNative.ghc
            ];
          };
      }
   );
}
