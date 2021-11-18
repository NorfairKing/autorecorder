let
  sources = import ./nix/sources.nix;
  pkgs = import ./nix/pkgs.nix { inherit sources; };
  pre-commit = import ./nix/pre-commit.nix { inherit sources; };

in
{
  inherit (pkgs) autorecorder;
  pre-commit-hooks = pre-commit.run;
} // pkgs.exampleCasts // pkgs.testCasts
