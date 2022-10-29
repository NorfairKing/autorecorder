final: prev:
with final.lib;
with final.haskell.lib;

let
  mkCastDerivationFunction = final.callPackage ./cast.nix { };
  mkCastDerivation = final.mkCastDerivationFunction { pkgs = final; };
in
{
  autorecorder = (justStaticExecutables final.haskellPackages.autorecorder).overrideAttrs (old: {
    passthru = (old.passtrhu or { }) // {
      inherit mkCastDerivationFunction mkCastDerivation;
    };
  });
  inherit mkCastDerivationFunction mkCastDerivation;
  haskellPackages = prev.haskellPackages.override (old: {
    overrides = final.lib.composeExtensions (old.overrides or (_: _: { })) (self: super: {
      autorecorder = generateOptparseApplicativeCompletion "autorecorder" (buildStrictly (self.callPackage ../autorecorder { }));
    });
  });
}
