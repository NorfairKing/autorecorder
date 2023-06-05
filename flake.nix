{
  description = "autorecorder";
  nixConfig = {
    extra-substituters = "https://autorecorder.cachix.org";
    extra-trusted-public-keys = "autorecorder.cachix.org-1:Im27BJ1dfw5yRVPqEL8ajnmLe7BHCldKEOyOWbtxEKM";
  };
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-23.05";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    validity.url = "github:NorfairKing/validity";
    validity.flake = false;
    dirforest.url = "github:NorfairKing/dirforest";
    dirforest.flake = false;
    autodocodec.url = "github:NorfairKing/autodocodec";
    autodocodec.flake = false;
    safe-coloured-text.url = "github:NorfairKing/safe-coloured-text";
    safe-coloured-text.flake = false;
  };
  outputs =
    { self
    , nixpkgs
    , pre-commit-hooks
    , validity
    , dirforest
    , autodocodec
    , safe-coloured-text
    }:
    let
      system = "x86_64-linux";
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
      overlays.${system} = import ./nix/overlay.nix;
      packages.${system}.default = pkgs.autorecorder;
      checks.${system} =
        let
          casts = import ./nix/test.nix { inherit pkgs; };
        in
        {
          casts = pkgs.linkFarmFromDrvs "casts" (pkgs.lib.attrValues casts);
          release = self.packages.${system}.default;
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
      devShells.${system}.default = pkgs.haskellPackages.shellFor {
        name = "autorecorder-shell";
        packages = (p:
          [ p.autorecorder ]
        );
        withHoogle = true;
        doBenchmark = true;
        buildInputs = (with pkgs; [
          zlib
          cabal-install
        ]) ++ (with pre-commit-hooks.packages.${system};
          [
            hlint
            hpack
            nixpkgs-fmt
            ormolu
            cabal2nix
          ]);
        shellHook = self.checks.${system}.pre-commit.shellHook;
      };
    };
}
