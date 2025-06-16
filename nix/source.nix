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
    rev = "c16c47328aece45b45a9c42c6323df01e249ed51";
    hash = "sha256-V6tPHev/bUNlDrd4m1yZyKG+aCdfHv8nxGK12Rivz6c=";
  };
  ghcjs-base = fetchFromGitHub {
    owner = "dmjio";
    repo = "ghcjs-base";
    rev = "ec79b37aec3d45264aa59306ff950632693c970c";
    hash = "sha256-0RkakDmeZZYscMfRhpGzKtscYSmE7H1sI2ldlrVksqY=";
  };
}
