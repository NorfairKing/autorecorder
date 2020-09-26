final: previous:
with final.haskell.lib;

{
  autorecorder = generateOptparseApplicativeCompletion "autorecorder" (
    failOnAllWarnings (
      disableLibraryProfiling (final.haskellPackages.callCabal2nix "autorecorder" (final.gitignoreSource ../.) {})
    )
  );
  intrayNotification = import ./notification.nix { pkgs = final; };
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
