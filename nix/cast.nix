{ pkgs ? import <nixpkgs> {}
}:
let
  fromYaml = yaml:
    builtins.fromJSON (
      builtins.readFile (
        pkgs.stdenv.mkDerivation {
          name = "fromYAML";
          phases = [ "buildPhase" ];
          buildPhase = "echo '${yaml}' | ${pkgs.yaml2json}/bin/yaml2json > $out";
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
        pkgs.stdenv.mkDerivation {
          inherit name;
          buildInputs = map (pkg: pkgs."${pkg}") (yamlContents.packages or []);
          buildCommand =
            let
              workingDirScript = pkgs.lib.optionalString (builtins.hasAttr "working-dir" yamlContents) ''
                mkdir -p ${yamlContents.working-dir}/.
                cp -r ${builtins.dirOf src + "/${yamlContents.working-dir}"}/. ${yamlContents.working-dir}/.
              '';
              # Note [Sanity]
              # This needs to be run on shell startup for backspace and enter to work
              # correctly but it cannot be run from a script beforehand because it
              # only works in (pseudo) terminals.
              bashRC = pkgs.writeText "bashrc" ''
                stty sane

                export PS1="\\$ "
                set -e
                set pipefail
              '';
            in
              ''
                # To make sure that the right colours are used.
                export TERM=xterm-256color

                # To make sure that backspace works, see Note [Sanity]
                export SHELL="${pkgs.bash}/bin/bash --rcfile ${bashRC}"

                # To make sure that programs like 'tree' show nice unicode characters
                export LANG=C.utf8
                export LC_ALL=C.utf8

                cp ${src} in.yaml

                # Set up the right working dir
                ${workingDirScript}

                # Record the cast
                ${pkgs.autorecorder}/bin/autorecorder record in.yaml "out.cast" \
                  --columns 80 \
                  --rows 25 \
                  --progress

                # Output the cast
                cp "out.cast" $out
              '';

        };
in
mkCastDerivation
