{
  description = "bcc-db-sync";

  inputs = {
    haskellNix.url = "github:The-Blockchain-Company/haskell.nix";
    tbcoNix = {
      url = "github:The-Blockchain-Company/tbco-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
    customConfig = {
      url = "path:./custom-config";
    };
  };

  outputs = { self, tbcoNix, haskellNix, nixpkgs, utils, customConfig, ... }:
    let
      inherit (haskellNix.internal) config;
      inherit (nixpkgs) lib;
      inherit (lib) systems mapAttrs recursiveUpdate mkDefault optionalAttrs nameValuePair
        attrNames getAttrs head;
      inherit (utils.lib) eachSystem mkApp flattenTree;
      inherit (tbcoNix.lib) prefixNamesWith collectExes;

      supportedSystems = import ./supported-systems.nix;
      defaultSystem = head supportedSystems;

      overlays = [
        haskellNix.overlay
        tbcoNix.overlays.haskell-nix-extra
        tbcoNix.overlays.crypto
        tbcoNix.overlays.bcc-lib
        tbcoNix.overlays.utils
        (final: prev: {
          customConfig = recursiveUpdate
            (import ./custom-config final.customConfig)
            customConfig.outputs;
          gitrev = self.rev or "dirty";
          commonLib = lib
            // tbcoNix.lib;
        })
        (import ./nix/pkgs.nix)
      ];

    in eachSystem supportedSystems (system:
      let
        pkgs = import nixpkgs { inherit system overlays config; };

        devShell = import ./shell.nix { inherit pkgs; };

        flake = pkgs.bccDbSyncProject.flake {};

        muslFlake = (import nixpkgs { inherit system overlays config;
          crossSystem = systems.examples.musl64;
        }).bccDbSyncProject.flake {};

        scripts = flattenTree pkgs.scripts;

        checkNames = attrNames flake.checks;

         checks =
          # checks run on default system only;
          optionalAttrs (system == defaultSystem) {
            hlint = pkgs.callPackage pkgs.hlintCheck {
              inherit (pkgs.bccDbSyncProject.projectModule) src;
            };
            stylish-haskell = pkgs.callPackage pkgs.stylishHaskellCheck {
              inherit (pkgs.bccDbSyncProject.projectModule) src;
            };
          };

        exes = collectExes flake.packages;
        exeNames = attrNames exes;
        lazyCollectExe = p: getAttrs exeNames (collectExes p);

        packages = {
          inherit (devShell) devops;
          inherit (pkgs) bcc-db-sync bcc-db-sync-extended bcc-node
            dockerImage;
        }
        // exes
        // (prefixNamesWith "static/"
              (mapAttrs pkgs.rewriteStatic (lazyCollectExe
                (if system == "x86_64-darwin" then flake else muslFlake).packages)))
        // scripts
        # Add checks to be able to build them individually
        // (prefixNamesWith "checks/" checks);

      in recursiveUpdate flake {

        inherit packages checks;

        legacyPackages = pkgs;

        # Built by `nix build .`
        defaultPackage = flake.packages."bcc-db-sync:exe:bcc-db-sync";

        # Run by `nix run .`
        defaultApp = flake.apps."bcc-db-sync:exe:bcc-db-sync";

        # This is used by `nix develop .` to open a devShell
        inherit devShell;

        apps = {
          repl = mkApp {
            drv = pkgs.writeShellScriptBin "repl" ''
              confnix=$(mktemp)
              echo "builtins.getFlake (toString $(git rev-parse --show-toplevel))" >$confnix
              trap "rm $confnix" EXIT
              nix repl $confnix
          '';
          };
          bcc-node = { type = "app"; program = pkgs.bcc-node.exePath; };
        } # nix run .#<exe>
        // (collectExes flake.apps);

      }
    ) // {
      overlay = final: prev: with self.legacyPackages.${final.system}; {
        inherit bcc-db-sync bcc-db-sync-extended bcc-node
            dockerImage;
      };
      nixosModules = {
        bcc-db-sync = { pkgs, lib, ... }: {
          imports = [ ./nix/nixos/bcc-db-sync-service.nix ];
          services.bcc-db-sync.dbSyncPkgs = lib.mkDefault self.legacyPackages.${pkgs.system};
        };
      };
    };
}
