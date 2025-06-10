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
    rev = "226657930f9a4abbd646a15497bab7a31f370167";
    hash = "sha256-EVSRRr7YaUOz9cgmsmwwFw08Wai6hxTdQV0/bQQvlfw=";
  };
  ghcjs-base = fetchFromGitHub {
    owner = "ghcjs";
    repo = "ghcjs-base";
    rev = "197b8bf05ba60ae0727bea7bcdb45f3c89778889";
    hash = "sha256-omKtlsevfYAQ9JAgTVwK0N3p7JFMuLs7AMbevF26w44=";
  };
}
