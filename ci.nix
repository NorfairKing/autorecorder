let
  pkgs = import ./nix/pkgs.nix;
in
{
  inherit (pkgs) autorecorder;
  pre-commit-hooks = (import ./nix/pre-commit-hooks.nix).run;
} // pkgs.exampleCasts // pkgs.testCasts
