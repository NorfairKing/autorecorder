{
  description = "autorecorder";
  nixConfig = {
    extra-substituters = "https://autorecorder.cachix.org";
    extra-trusted-public-keys = "autorecorder.cachix.org-1:Im27BJ1dfw5yRVPqEL8ajnmLe7BHCldKEOyOWbtxEKM";
  };
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-25.05";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
  };
  outputs =
    { self
    , nixpkgs
    , pre-commit-hooks
    }:
    let
      system = "x86_64-linux";
      pkgsFor = nixpkgs: import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = [
          self.overlays.${system}
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
        packages = p: [ p.autorecorder ];
        withHoogle = true;
        doBenchmark = true;
        buildInputs = with pkgs; [
          zlib
          cabal-install
        ] ++ self.checks.${system}.pre-commit.enabledPackages;
        shellHook = self.checks.${system}.pre-commit.shellHook;
      };
    };
}
