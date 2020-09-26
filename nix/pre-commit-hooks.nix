let
  nix-pre-commit-hooks = import (
    builtins.fetchGit {
      url = "https://github.com/cachix/pre-commit-hooks.nix";
      rev = "4dd50ef441796b439a56f1e0f8b127d4129f8947";
      ref = "master";
    }
  );
in
{
  run =
    nix-pre-commit-hooks.run {
      src = ../.;
      hooks = {
        nixpkgs-fmt.enable = true;
        ormolu.enable = true;
        hlint.enable = true;
      };
    };
  tools = [
    nix-pre-commit-hooks.ormolu
    nix-pre-commit-hooks.nixpkgs-fmt
    nix-pre-commit-hooks.hlint
  ];
}
