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

  # bundler script, invokes rspack to bundle main.lynx.bundle.
  counter-bundle = super.writeScriptBin "counter-bundle" ''
    set -x
    if [ ! -f dist/ ]; then
      rm -rv dist/
    fi
    cabal clean
    mkdir -pv dist/
    bun run js
    cp -v $(nix-build -A miso-lynx-examples-ghcjs9122)/bin/counter.jsexe/all.js .
    chmod +rw all.js
    bun build --minify all.js --target=bun --outfile=dist/all.js
    file dist/all.js
    bun run bundle
    file dist/main.lynx.bundle
  '';

  # for the flake.nix
  ghc-native = self.haskell.packages.ghcNative.ghc;

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
