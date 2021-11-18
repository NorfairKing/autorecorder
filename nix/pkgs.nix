{ sources ? import ./sources.nix
}:
let
  pkgsv = import sources.nixpkgs;
  pkgs = pkgsv {};

in
pkgsv {
  overlays =
    [
      (import (sources.dirforest + "/nix/overlay.nix"))
      (import (sources.autodocodec + "/nix/overlay.nix"))
      (import (sources.safe-coloured-text + "/nix/overlay.nix"))
      (import ./gitignore-src.nix)
      (import ./overlay.nix)
    ];
  config.allowUnfree = true;
}
