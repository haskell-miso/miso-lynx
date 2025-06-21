{
  description = "A flake for acquiring GHCJS 9.12.2 w/ modded w/ the JSBI.patch";
  inputs = {
    nixpkgs.url =
      "https://github.com/alexfmpe/nixpkgs/archive/b594b289740a2bc917ed9c66fef5d905f389cb96.tar.gz";
  };
  outputs = { self, nixpkgs }: {
    devShells.x86_64-linux.default =
      let
        system = "x86_64-linux";
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ (import ./nix/overlay.nix) ];
          config = {
            allowUnfree = true;
            allowBroken = false;
          };
        };
      in
        pkgs.mkShell {
          buildInputs = with pkgs; [
            ghc-native
          ];
        };
  };
}
