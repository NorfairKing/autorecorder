let
  pkgsv = import (import ./nixpkgs.nix);
  pkgs = pkgsv {};
  yamlparse-applicative-overlay =
    import (
      pkgs.fetchFromGitHub (import ./yamlparse-applicative-version.nix) + "/nix/overlay.nix"
    );
  dirforest-overlay =
    import (
      builtins.fetchGit (import ./dirforest-version.nix) + "/nix/overlay.nix"
    );

in
pkgsv {
  overlays =
    [
      yamlparse-applicative-overlay
      dirforest-overlay
      (import ./gitignore-src.nix)
      (import ./overlay.nix)
    ];
  config.allowUnfree = true;
}
