{ lib, fetchFromGitHub, fetchgit, fetchzip, ... }:
with lib;
with (builtins.fromJSON (builtins.readFile ../flake.lock));
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
  # fetch from flake
  fetchFromFlake = args:
    fetchFromGitHub {
      inherit (args.locked) owner repo rev;
      hash = args.locked.narHash;
    };
in
{
  miso-lynx = make-src-filter ../.;
  examples = make-src-filter ../examples;
  miso = fetchFromFlake (nodes.miso);
  jsaddle = fetchFromFlake (nodes.jsaddle);
  ghcjs-base = fetchFromGitHub {
    owner = "dmjio";
    repo = "ghcjs-base";
    rev = "ec79b37aec3d45264aa59306ff950632693c970c";
    hash = "sha256-0RkakDmeZZYscMfRhpGzKtscYSmE7H1sI2ldlrVksqY=";
  };
}
