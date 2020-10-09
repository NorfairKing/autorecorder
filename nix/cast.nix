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
              calculateDirs = srcDir: dir:
                if builtins.baseNameOf dir == ".."
                then {
                  includeDirSource = "..";
                  includeDirDestination = ".";
                  cdDir = ".";
                }
                else {
                  includeDirSource = dir;
                  includeDirDestination = dir;
                  cdDir = dir;
                };
              makeWorkingDirScript = dir:
                let
                  srcDir = builtins.dirOf src;
                  dirs = calculateDirs srcDir dir;
                  dirToInclude = srcDir + "/${dirs.includeDirSource}";
                in
                  ''
                    # set -x
                    # echo dirToInclude ${dirToInclude}
                    # echo cdDir ${dirs.cdDir}
                    mkdir -p "$(dirname ${dirs.includeDirDestination})" # To make sure that the parent of the destination exists.
                    ${pkgs.rsync}/bin/rsync -r ${dirToInclude}/ ${dirs.includeDirDestination}
                    # ${pkgs.tree}/bin/tree
                    cd ${dirs.cdDir}
                    # set +x
                  '';
              workingDirScript = pkgs.lib.optionalString
                (builtins.hasAttr "working-dir" yamlContents)
                (makeWorkingDirScript yamlContents.working-dir);
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

                # Set up a playground directory because otherwise there will be an envvar file
                playground="casting-ground"
                mkdir "$playground"
                cd "$playground"

                # Set up the right working dir
                ${workingDirScript}
                
                # Record the cast
                ${pkgs.autorecorder}/bin/autorecorder record "${src}" "$out" \
                  --working-dir "$(pwd)" \
                  --progress
              '';

        };
in
mkCastDerivation
