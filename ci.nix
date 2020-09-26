let
  pkgs = import ./nix/pkgs.nix;
in
{
  inherit (pkgs) autorecorder;
  pre-commit-check = import ./nix/pre-commit-hooks.nix;
}
