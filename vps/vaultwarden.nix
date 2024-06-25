{ pkgs, config, ... }:
with builtins;
let
vaultCfg = config.services.vaultwarden.config;
hostName = config.networking.hostName;
in {
    services.nginx.virtualHosts = {
        "VaultWarden" = {
            serverName = replaceStrings ["https://" "http://"] ["" ""] vaultCfg.DOMAIN;
            enableACME = true;
            forceSSL = true;
            locations."/" = {
                proxyPass = "http://${vaultCfg.ROCKET_ADDRESS}:${toString vaultCfg.ROCKET_PORT}";
            };
        };
    };

    services.vaultwarden = {
        enable = true;
        backupDir = "/srv/vaultwarden-backup";
        config = {
            DOMAIN = "https://vault.${hostName}";
            SIGNUPS_ALLOWED = false;
            ROCKET_ADDRESS = "127.0.0.1";
            ROCKET_PORT = 8000;
            ROCKET_LOG = "critical";
        };
    };
}
