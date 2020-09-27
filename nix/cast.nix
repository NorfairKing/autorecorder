{ stdenv
, bash
, yaml2json
, writeText
, gitignoreSource
, autorecorder
}:
let
  fromYaml = yaml:
    builtins.fromJSON (
      builtins.readFile (
        stdenv.mkDerivation {
          name = "fromYAML";
          phases = [ "buildPhase" ];
          buildPhase = "echo '${yaml}' | ${yaml2json}/bin/yaml2json > $out";
        }
      )
    );
  mkCastDerivation =
    { name
    , src
    }:
      let
        yamlContents = fromYaml (builtins.readFile src);
      in
        builtins.trace (yamlContents) stdenv.mkDerivation {
          inherit name;
          buildCommand =
            let
              # This needs to be run on shell startup for backspace and enter to work
              # correctly but it cannot be run from a script beforehand because it
              # only works in (pseudo) terminals.
              bashRC = writeText "bashrc" ''
                stty sane

                export PS1="\\$ "
                set -e
                set pipefail
              '';
            in
              ''
                # To make sure that the right colours are used.
                export TERM=xterm-256color

                # To make sure that backspace works, see above
                export SHELL="${bash}/bin/bash --rcfile ${bashRC}"

                # To make sure that programs like 'tree' show nice unicode characters
                export LANG=C.utf8
                export LC_ALL=C.utf8

                # Record the cast
                ${autorecorder}/bin/autorecorder record ${src} "out.cast" \
                  --columns 80 \
                  --rows 25 \
                  --progress

                # Output the cast
                cp "out.cast" $out
              '';

        };
in
mkCastDerivation
