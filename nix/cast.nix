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
              # This piece is surprisingly complex.
              # The spec file can specify a working dir for the cast to happen in, relative to the directory where the spec file is.
              # When the working dir is lower than the spec file, that working dir needs to exist during the build so that 
              # autorecorder can cd into it.
              # When the working dir is higher than the spec file, the parent directories need to exist during the build so that
              # autorecorder can cd up to them.
              # The following table describes the situation in all its complexity.
              # 
              #  Spec File             | Working dir | Copy             | Casting Start   | Casting `cd`s to | Implemented
              # -----------------------+-------------+------------------+-----------------+------------------+-------------
              #  ./foo/bar/spec.yaml   | .           | /build/          | /build/         | /build/          | yes   
              #  ./foo/bar/spec.yaml   | baz         | /build/baz       | /build/         | /build/baz       | yes   
              #  ./foo/bar/spec.yaml   | baz/quux    | /build/baz/quux  | /build/         | /build/baz/quux  | yes   
              #  ./foo/bar/spec.yaml   | ..          | /build/bar       | /build/bar      | /build/          | no   
              #  ./foo/bar/spec.yaml   | ../..       | /build/foo/bar   | /build/foo/bar  | /build/          | no    
              #  ./foo/bar/spec.yaml   | ../xyz      | /build/foo/xyz   | /build/bar      | /build/xyz       | no    
              #
              workingDirScript = pkgs.lib.optionalString (builtins.hasAttr "working-dir" yamlContents) ''
                mkdir -p ${yamlContents.working-dir}
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

                # Set up the right working dir
                ${workingDirScript}
                
                # Get the cast file ready
                cp ${src} in.yaml

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
