let
  pkgs = import ./nix/pkgs.nix {};
in
{
  inherit (pkgs) autorecorder;
}
