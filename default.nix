{ overlays ? []
}:
with (import ./nix { inherit overlays; });

with pkgs.haskell.lib;
{
  inherit pkgs;

  # hackage release
  release =
    with pkgs.haskell.packages.ghc9122;
    sdistTarball (buildStrictly miso-lynx);

  # hackage release examples
  release-examples =
    with pkgs.haskell.packages.ghc9122;
    sdistTarball (buildStrictly miso-lynx-examples);

  # lynx (iOS and Android)
  #
  # (pkgsCross.ghcs....ghcLynx is just pkgsCross.ghcjs...ghc9122,
  # but compiled w/ -sENVIRONMENT=shell).
  miso-lynx-examples-ghcjs9122 =
    pkgs.pkgsCross.ghcjs.haskell.packages.ghcNative.miso-lynx-examples;

  # dmj: Build lynx iOS and Android apps with Haskell and Lynx
  miso-lynx =
    pkgs.pkgsCross.ghcjs.haskell.packages.ghcNative.miso-lynx;

  miso-lynx-ghc9122 =
    pkgs.haskell.packages.ghc9122.miso-lynx;

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
