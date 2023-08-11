let
  pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/90470003541507c3a419c84a9f22f5f78e1acbdc.tar.gz") {};
in
pkgs.mkShell {
  packages = [
    pkgs.caddy
    pkgs.elmPackages.elm
    pkgs.elmPackages.elm-format
    pkgs.elmPackages.elm-optimize-level-2
    pkgs.elmPackages.elm-test
    pkgs.shellcheck
  ];

  shellHook =
    ''
    export project="$PWD"

    export PATH="$project/bin:$PATH"
    '';
}
