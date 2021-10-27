{ system ? builtins.currentSystem
, crossSystem ? null
# allows to cutomize haskellNix (ghc and profiling, see ./nix/haskell.nix)
, config ? {}
# override scripts with custom configuration
, customConfig ? {}
# allows to override dependencies of the project without modifications,
# eg. to test build against local checkout of nixpkgs and tbco-nix:
# nix build -f default.nix bcc-db-sync --arg sourcesOverride '{
#   tbco-nix = ../tbco-nix;
# }'
, sourcesOverride ? {}
# pinned version of nixpkgs augmented with overlays (tbco-nix and our packages).
, pkgs ? import ./nix { inherit system crossSystem config sourcesOverride gitrev customConfig; }
, gitrev ? null
}:
with pkgs; with commonLib;
let

  haskellPackages = recRecurseIntoAttrs
      # the Haskell.nix package set, reduced to local packages.
      (selectProjectPackages bccDbSyncHaskellPackages);

  packages = {
    inherit haskellPackages bcc-db-sync bcc-db-sync-extended bcc-node scripts dockerImage;

    # so that eval time gc roots are cached (nix-tools stuff)
    inherit (bccDbSyncProject) roots plan-nix;

    inherit (haskellPackages.bcc-db-sync.identifier) version;

    exes = mapAttrsRecursiveCond (as: !(isDerivation as)) rewriteStatic (collectComponents' "exes" haskellPackages);

    # `tests` are the test suites which have been built.
    tests = collectComponents' "tests" haskellPackages;
    # `benchmarks` (only built, not run).
    benchmarks = collectComponents' "benchmarks" haskellPackages;

    checks = recurseIntoAttrs {
      # `checks.tests` collect results of executing the tests:
      tests = collectChecks haskellPackages;

      hlint = callPackage hlintCheck {
        inherit (pkgs.bccDbSyncProject.projectModule) src;
      };

      stylish-haskell = callPackage stylishHaskellCheck {
        inherit (pkgs.bccDbSyncProject.projectModule) src;
      };
    };

    shell = import ./shell.nix {
      inherit pkgs;
      withHoogle = true;
    };
};
in packages
