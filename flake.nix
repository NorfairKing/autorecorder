{
  description = "autorecorder";
  nixConfig = {
    extra-substituters = "https://autorecorder.cachix.org";
    extra-trusted-public-keys = "autorecorder.cachix.org-1:Im27BJ1dfw5yRVPqEL8ajnmLe7BHCldKEOyOWbtxEKM";
  };
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-22.05";
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    validity.url = "github:NorfairKing/validity?ref=flake";
    validity.flake = false;
    dirforest.url = "github:NorfairKing/dirforest?ref=flake";
    dirforest.flake = false;
    autodocodec.url = "github:NorfairKing/autodocodec?ref=flake";
    autodocodec.flake = false;
    safe-coloured-text.url = "github:NorfairKing/safe-coloured-text?ref=flake";
    safe-coloured-text.flake = false;
  };
  outputs =
    { self
    , nixpkgs
    , flake-utils
    , pre-commit-hooks
    , validity
    , dirforest
    , autodocodec
    , safe-coloured-text
    }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ]
      (system:
      let
        pkgsFor = nixpkgs: import nixpkgs {
          inherit system;
          config.allowUnfree = true;
          overlays = [
            self.overlays.${system}
            (import (autodocodec + "/nix/overlay.nix"))
            (import (safe-coloured-text + "/nix/overlay.nix"))
            (import (validity + "/nix/overlay.nix"))
            (import (dirforest + "/nix/overlay.nix"))
          ];
        };
        pkgs = pkgsFor nixpkgs;
      in
      {
        overlays = import ./nix/overlay.nix;
        packages.default = pkgs.autorecorder;
        checks =
          let
            casts = import ./nix/test.nix { inherit pkgs; autorecorder = pkgs.autorecorder; };
          in
          {
            casts = pkgs.symlinkJoin { name = "casts"; paths = pkgs.lib.attrValues casts; };
            pre-commit = pre-commit-hooks.lib.${system}.run {
              src = ./.;
              hooks = {
                hlint.enable = true;
                hpack.enable = true;
                ormolu.enable = true;
                nixpkgs-fmt.enable = true;
                nixpkgs-fmt.excludes = [ ".*/default.nix" ];
                cabal2nix.enable = true;
              };
            };
          };
        devShells.default = pkgs.haskellPackages.shellFor {
          name = "autorecorder-shell";
          packages = (p:
            [ p.autorecorder ]
          );
          withHoogle = true;
          doBenchmark = true;
          buildInputs = with pkgs; [
            niv
            zlib
            cabal-install
          ] ++ (with pre-commit-hooks;
            [
              hlint
              hpack
              nixpkgs-fmt
              ormolu
              cabal2nix
            ]);
          shellHook = self.checks.${system}.pre-commit.shellHook;
        };
      });
}
