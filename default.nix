{ overlays ? []
}:
with (import ./nix { inherit overlays; });

with pkgs.haskell.lib;
{
  inherit pkgs;

  # hackage release
  release =
    with pkgs.haskell.packages.ghc9122;
    sdistTarball (buildStrictly miso-native);

  # hackage release examples
  release-examples =
    with pkgs.haskell.packages.ghc9122;
    sdistTarball (buildStrictly miso-native-examples);

  # native (iOS and Android)
  #
  # (pkgsCross.ghcs....ghcNative is just pkgsCross.ghcjs...ghc9122,
  # but compiled w/ -sENVIRONMENT=shell).
  miso-native-examples-ghcjs9122 =
    pkgs.pkgsCross.ghcjs.haskell.packages.ghcNative.miso-native-examples;

  # dmj: Build native iOS and Android apps with Haskell and Lynx
  miso-native =
    pkgs.pkgsCross.ghcjs.haskell.packages.ghcNative.miso-native;

  miso-native-ghc9122 =
    pkgs.haskell.packages.ghc9122.miso-native;

  # haddocks
  inherit (pkgs)
    haddocks;

  # ghciwatch
  inherit (pkgs)
    ghciwatch;

  # bun
  inherit (pkgs)
    bun;

  # counter-bundle
  # dmj: this is a script to build a lynx bundle for the counter app, built w/ nix.
  # for use in CI
  inherit (pkgs)
    counter-bundle;

  # nurl
  # $ nurl https://github.com/nix-community/nurl
  #
  # fetchFromGitHub {
  #   owner = "nix-community";
  #   repo = "nurl";
  #   rev = "3a3ba7f0d14d92e1266395d826c6e229797d0044";
  #   hash = "sha256-WAFqmlsShuQngk6LMFlgz7Oyc41TAQeTa/49phhRizY=";
  # }
  #
  inherit (pkgs)
    nurl;

}
