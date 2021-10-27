# our packages overlay
final: prev: with final;
let
  compiler = config.haskellNix.compiler or "ghc8104";
in {
  src = haskell-nix.haskellLib.cleanGit {
    src = ../.;
    name = "bcc-db-sync";
  };

  schema = haskell-nix.haskellLib.cleanGit {
    src = ../.;
    subDir = "schema";
    name = "bcc-db-sync-schema";
  };

  bccDbSyncProject = callPackage ./haskell.nix {
    inherit compiler;
  };

  bccDbSyncHaskellPackages = bccDbSyncProject.hsPkgs;

  # Grab the executable component of our package.
  inherit (bccDbSyncHaskellPackages.bcc-db-sync.components.exes)
      bcc-db-sync;
  inherit (bccDbSyncHaskellPackages.bcc-db-sync-extended.components.exes)
      bcc-db-sync-extended;
  inherit (bccDbSyncHaskellPackages.bcc-db-tool.components.exes)
    bcc-db-tool;
  inherit (bccDbSyncHaskellPackages.bcc-node.components.exes)
      bcc-node;

  cabal = haskell-nix.tool compiler "cabal" {
    version = "latest";
    inherit (bccDbSyncProject) index-state;
  };

  hlint = haskell-nix.tool compiler "hlint" {
    version = "3.2.7";
    inherit (bccDbSyncProject) index-state;
  };

  stylish-haskell = haskell-nix.tool compiler "stylish-haskell" {
    version = "latest";
    inherit (bccDbSyncProject) index-state;
  };

  # systemd can't be statically linked:
  postgresql = prev.postgresql.override {
    enableSystemd = stdenv.hostPlatform.isLinux && !stdenv.hostPlatform.isMusl;
  };

  scripts = import ./scripts.nix { inherit pkgs; };

  dockerImage = let
    defaultConfig = rec {
      services.bcc-db-sync = {
        restoreSnapshot = lib.mkDefault "$RESTORE_SNAPSHOT";
        socketPath = lib.mkDefault ("/node-ipc/node.socket");
        postgres.generatePGPASS = false;
      };
    };
  in callPackage ./docker.nix {
    scripts = import ./scripts.nix {
      inherit pkgs;
      customConfigs = [ defaultConfig customConfig ];
    };
    extendedScripts = import ./scripts.nix {
      inherit pkgs;
      customConfigs = [
        defaultConfig customConfig
        { services.bcc-db-sync.extended = true; }
      ];
    };
  };
}
