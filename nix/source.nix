{ lib, fetchFromGitHub, fetchgit, fetchzip, ... }:
with lib;
let
  make-src-filter = src: with lib;
    cleanSourceWith {
      inherit src;
      filter =
        name: type: let baseName = baseNameOf (toString name); in
         ((type == "regular" && hasSuffix ".hs" baseName) ||
         (hasSuffix ".yaml" baseName) ||
         (hasSuffix ".cabal" baseName) ||
         (hasSuffix ".css" baseName) ||
         (hasSuffix ".html" baseName) ||
         (hasSuffix ".png" baseName) ||
         (hasSuffix ".js" baseName) ||
         (hasSuffix ".ts" baseName) ||
         (hasSuffix ".json" baseName) ||
         (baseName == "README.md") ||
         (baseName == "LICENSE") ||
         (type == "directory" && baseName != "examples") ||
         (type == "directory" && baseName != "dist"));
    };
in
{
  miso-native = make-src-filter ../.;
  examples = make-src-filter ../examples;
  jsaddle = fetchFromGitHub {
    owner = "ghcjs";
    repo = "jsaddle";
    rev = "0d5e427cb99391179b143dc93dfbac9c1019237b";
    sha256 = "sha256-jyJ7bdz0gNLOSzRxOWcv7eWGIwo3N/O4PcY7HyNF8Fo=";
  };
  miso = fetchFromGitHub {
    owner = "dmjio";
    repo = "miso";
    rev = "c694abcce1d0508bb47c409b5de1cf321a714157";
    hash = "sha256-EL35cm0gCd7KV+48K3pCIfVjoFJ6B0PZD6nbW6UfSX8=";
  };
  ghcjs-base = fetchFromGitHub {
    owner = "dmjio";
    repo = "ghcjs-base";
    rev = "ec79b37aec3d45264aa59306ff950632693c970c";
    hash = "sha256-0RkakDmeZZYscMfRhpGzKtscYSmE7H1sI2ldlrVksqY=";
  };
}
