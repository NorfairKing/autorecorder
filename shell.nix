let
  pkgs = import ./nix/pkgs.nix;
  pre-commit = import ./nix/pre-commit-hooks.nix;

in
pkgs.mkShell {
  buildInputs = pre-commit.tools;
  shellHook = ''
    ${pre-commit.run.shellHook}
  '';
}
