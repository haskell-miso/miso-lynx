{
  description = "miso-lynx — the miso-lynx counter example, built against miso's Miso.Native (LynxJS dual-thread) backend";

  inputs = {
    miso.url = "github:dmjio/miso";
  };
  # For local miso development (with uncommitted changes), override the input to
  # a local checkout — note the ABSOLUTE path (relative `path:../miso` doesn't
  # resolve correctly from a git flake):
  #
  #   nix build --override-input miso path:/absolute/path/to/miso

  outputs = inputs:
    let miso = inputs.miso;
    in miso.inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        inherit (miso.inputs.nixpkgs) lib;   # for lib.cleanSource

        # miso's GHC-JS (LynxJS) package set, with miso-native — exposed by miso's
        # flake, so we don't re-import nixpkgs + the overlay here.
        ghcNative = miso.lib.${system}.ghcNative;

        # The counter example, compiled with the GHC JavaScript backend against
        # miso's native (-fnative) build.
        miso-lynx-examples =
          ghcNative.callCabal2nix "miso-lynx-examples" (lib.cleanSource ./examples) {
            miso = ghcNative.miso-native;
          };

        # The counter's Lynx bundle, via miso's shared helper: minifies all.js
        # and compiles in styles.css.
        counter-bundle = miso.lib.${system}.mkLynxBundle {
          name = "miso-lynx-counter-bundle";
          jsDrv = miso-lynx-examples;
          exeName = "counter";
        };
      in
      {
        # `nix build` -> result/main.lynx.bundle
        packages = {
          default = counter-bundle;
          inherit miso-lynx-examples counter-bundle;
        };

        # Inherit miso's dev shells (toolchain: GHC JS backend, bun, rspeedy).
        devShells = {
          default = miso.devShells.${system}.default;
          native = miso.devShells.${system}.native;
          wasm = miso.devShells.${system}.wasm;
        };
      });
}
