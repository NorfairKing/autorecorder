{ pkgs ? import ./pkgs.nix {}
}:
let
  mkCastDerivation =
    { name
    , src
    , debug ? false
    , default-rows ? null
    , default-columns ? null
    }:
      let
        localPkgs = import ./pkgs.nix;
        autorecorder = localPkgs.haskellPackages.autorecorder;
        yamlContents =
          builtins.fromJSON (
            builtins.readFile (
              localPkgs.stdenv.mkDerivation {
                name = "fromYAML";
                phases = [ "buildPhase" ];
                buildPhase = "${localPkgs.yq}/bin/yq 'del(.input)' ${src} > $out";
              }
            )
          );
      in
        localPkgs.stdenv.mkDerivation {
          inherit name;
          buildInputs = map (pkg: pkgs."${pkg}") (yamlContents.packages or []);
          buildCommand = with localPkgs.lib;
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
              #  ./foo/bar/spec.yaml   | ..          | /build/bar       | /build/bar      | /build/          | yes
              #  ./foo/bar/spec.yaml   | ../..       | /build/foo/bar   | /build/foo/bar  | /build/          | yes
              #  ./foo/bar/spec.yaml   | ../xyz      | /build/foo/xyz   | /build/bar      | /build/xyz       | yes
              #
              calculateDirs = srcDir: dir:
                if dir == "."
                then {
                  includeDirSource = ".";
                  includeDirDestination = ".";
                  cdDir = ".";
                }
                else
                  let
                    dirs = calculateDirs srcDir (builtins.dirOf dir);
                    base = builtins.baseNameOf dir;
                  in
                    if base == ".."
                    then {
                      includeDirSource = dirs.includeDirSource + "/..";
                      includeDirDestination = dirs.includeDirDestination + "/.";
                      cdDir = dirs.cdDir;
                    }
                    else {
                      includeDirSource =
                        if dirs.includeDirSource != "."
                        then dirs.includeDirSource + "/${base}"
                        else dirs.includeDirSource;
                      includeDirDestination =
                        if dirs.includeDirDestination != "."
                        then dirs.includeDirDestination + "/${base}"
                        else dirs.includeDirDestination;
                      cdDir = dirs.cdDir + "/${base}";
                    };
              makeWorkingDirScript = dir:
                let
                  srcDir = builtins.dirOf src;
                  dirs = calculateDirs srcDir dir;
                  dirToInclude = srcDir + "/${dirs.includeDirSource}";
                in
                  ''
                    ${optionalString debug ''
                    echo "Showing a lot more info because debug is on."
                    set -x
                    echo dirToInclude ${dirToInclude}
                    echo cdDir ${dirs.cdDir}
                  ''}

                    # Make sure that the parent of the destination exists.
                    mkdir -p "$(dirname ${dirs.includeDirDestination})"

                    # Make extra sure that copying will succeed
                    chmod -R 755 .
                    chmod -R 755 "$(dirname ${dirs.includeDirDestination})"
  
                    # Copy over the required context
                    ${localPkgs.rsync}/bin/rsync --no-p --no-g --chmod=ugo=rwX -r ${dirToInclude}/ ${dirs.includeDirDestination}

                    ${optionalString debug "${localPkgs.tree}/bin/tree"}

                    # Move into the right dir
                    cd ${dirs.cdDir}

                    chmod -R 755 .
                    ${optionalString debug "set +x"}
                  '';
              workingDirScript = optionalString
                (builtins.hasAttr "working-dir" yamlContents)
                (makeWorkingDirScript yamlContents.working-dir);
              # Note [Sanity]
              # This needs to be run on shell startup for backspace and enter to work
              # correctly but it cannot be run from a script beforehand because it
              # only works in (pseudo) terminals.
              bashRC = localPkgs.writeText "bashrc" ''
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
                export SHELL="${localPkgs.bash}/bin/bash --rcfile ${bashRC}"

                # To make sure that programs like 'tree' show nice unicode characters
                export LANG=C.utf8
                export LC_ALL=C.utf8

                # Set up a playground directory because otherwise there will be an envvar file
                playground="casting-ground"
                mkdir "$playground"
                chmod -R 755 "$playground"
                cd "$playground"

                # Set up the right working dir
                ${workingDirScript}
                
                # Record the cast
                ${autorecorder}/bin/autorecorder record "${src}" "$out" \
                  --working-dir "$(pwd)" ${optionalString (! builtins.isNull default-rows) "--default-rows ${builtins.toString default-rows}"} ${optionalString (! builtins.isNull default-columns) "--default-columns ${builtins.toString default-columns}"} \
                  --no-cleanup \
                  ${if debug then "--debug" else "--progress"}
              '';

        };
in
mkCastDerivation
