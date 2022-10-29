{ pkgs
}:
with pkgs.lib;
let
  mkCastDerivation = pkgs.autorecorder.passthru.mkCastDerivation;
  exampleCasts =
    let
      specFiles = builtins.map (removeSuffix ".yaml")
        (
          builtins.attrNames
            (
              filterAttrs
                (p: v: v == "regular" && hasSuffix ".yaml" p)
                (builtins.readDir ../examples)
            )
        );
    in
    listToAttrs (
      map
        (
          file: nameValuePair "${file}-cast" (
            mkCastDerivation {
              name = "${file}-cast";
              src = ../examples/${file}.yaml;
              # debug = true;
            }
          )
        )
        specFiles
    );
  testCasts =
    let
      mkTest = name: path:
        mkCastDerivation {
          inherit name;
          src = path;
          # debug = true;
        };
    in
    mapAttrs mkTest {
      "false-cast" = ../test-casts/false/spec.yaml;
      "current-dir-cast" = ../test-casts/current-dir/spec.yaml;
      "subdir-cast" = ../test-casts/subdir/spec.yaml;
      "subsubdir-cast" = ../test-casts/subsubdir/spec.yaml;
      "subsubsubdir-cast" = ../test-casts/subsubsubdir/spec.yaml;
      "supdir-cast" = ../test-casts/supdir/subdir/spec.yaml;
      "supsupdir-cast" = ../test-casts/supsupdir/subdir/subsubdir/spec.yaml;
      "supsupsupdir-cast" = ../test-casts/supsupsupdir/subdir/subsubdir/subsubsubdir/spec.yaml;
      "updown-cast" = ../test-casts/updown/up/spec.yaml;
      "upupdown-cast" = ../test-casts/upupdown/up1/up2/spec.yaml;
      "updowndown-cast" = ../test-casts/updowndown/up/spec.yaml;
    };
in
exampleCasts // testCasts
