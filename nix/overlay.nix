final: previous:
with final.lib;
with final.haskell.lib;

{
  autorecorder = generateOptparseApplicativeCompletion "autorecorder" (
    failOnAllWarnings (
      disableLibraryProfiling (final.haskellPackages.callCabal2nix "autorecorder" (final.gitignoreSource ../autorecorder) {})
    )
  );
  mkCastDerivationFunction = import ./cast.nix;
  mkCastDerivation = final.mkCastDerivationFunction { pkgs = final; };
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
        map (
          file: nameValuePair "${file}-cast" (
            final.mkCastDerivation {
              name = "${file}-cast";
              src = ../examples + "/${file}.yaml";
              # debug = true;
            }
          )
        ) specFiles
      );
  testCasts =
    let
      mkTest = name: path:
        final.mkCastDerivation {
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
  haskellPackages =
    previous.haskellPackages.override (
      old:
        {
          overrides =
            final.lib.composeExtensions (old.overrides or (_: _: {})) (
              self: super:
                let
                  dirforestRepo =
                    final.fetchFromGitHub {
                      owner = "NorfairKing";
                      repo = "dirforest";
                      rev = "8dba4d4a5be45c293a33480264e55d2cb8b71d3f";
                      sha256 =
                        "sha256:0am5d074zsyrjbmv8acqbwc6dp7ysrz8xznvwkzq66nn990rmxmm";
                    };
                  dirforestPkg =
                    name:
                      self.callCabal2nix name (dirforestRepo + "/${name}") {};
                  dirforestPkgs =
                    {
                      "dirforest" = dirforestPkg "dirforest";
                    };

                in
                  {
                    inherit (final) autorecorder;
                  } // dirforestPkgs
            );
        }
    );
}
