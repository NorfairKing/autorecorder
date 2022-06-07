{ sources ? import ./sources.nix
}:
let
  pkgsv = import sources.nixpkgs;
in
pkgsv {
  overlays =
    [
      (import (sources.dirforest + "/nix/overlay.nix"))
      (import (sources.autodocodec + "/nix/overlay.nix"))
      (import (sources.validity + "/nix/overlay.nix"))
      (final: previous: { inherit (import sources.gitignore { inherit (final) lib; }) gitignoreSource; })
      (import ./overlay.nix)
    ];
}
