self: super: {

  # ghci watcher
  ghciwatch =
    (builtins.getFlake "github:MercuryTechnologies/ghciwatch")
      .outputs.packages."${super.system}".ghciwatch;

  # dmj: Ensure you call 'nix-shell --run 'cabal haddock-project'' first
  # this happens in CI
  haddocks = self.stdenv.mkDerivation {
    name = "haddocks";
    src = ../haddocks;
    buildCommand = ''
      mkdir -p $out
      cp -rv $src/* $out
    '';
  };

  # haskell stuff
  haskell = super.haskell // {
    packages = super.haskell.packages // {
      ghc9122 = super.haskell.packages.ghc9122.override {
        overrides = import ./haskell/packages/ghc self;
      };
      ghcNative = super.haskell.packages.ghc9122.override {
        overrides = import ./haskell/packages/native self;
      };
    };
  };
}
