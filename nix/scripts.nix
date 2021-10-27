{ pkgs
, customConfigs ? [ pkgs.customConfig ] }:
let
  inherit (pkgs) lib bccLib;
  inherit (pkgs.commonLib) evalService;
  blacklistedEnvs = [ "selfnode" "sophie_selfnode" "latency-tests" "mainnet-ci" ];
  environments = lib.filterAttrs (k: v: (!builtins.elem k blacklistedEnvs)) bccLib.environments;
  mkScript = envConfig: let
    service = evalService {
      inherit pkgs customConfigs;
      serviceName = "bcc-db-sync";
      modules = [
        ./nixos/bcc-db-sync-service.nix
        {
          services.bcc-db-sync = {
            postgres.user = lib.mkDefault "*";
            environment = lib.mkDefault envConfig;
            cluster = lib.mkDefault envConfig.name;
            dbSyncPkgs = lib.mkDefault pkgs;
          };
        }
      ];
    };
  in lib.recurseIntoAttrs {
    db-sync = pkgs.writeScriptBin "bcc-db-sync-${service.cluster}" ''
      #!${pkgs.runtimeShell}
      set -euo pipefail
      ${service.script} $@
    '' // {
      passthru = { inherit service; };
    };
  };
in bccLib.forEnvironmentsCustom mkScript environments
