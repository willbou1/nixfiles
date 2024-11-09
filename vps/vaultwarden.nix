{
  pkgs,
  config,
  ...
}:
with builtins; let
  vaultCfg = config.services.vaultwarden.config;
  hostName = config.networking.hostName;
  suffix = config.networking.suffix;
  vaultwardenAddress = "vault.${hostName}.${suffix}";
in {
  security.acme.certs."${hostName}.${suffix}".extraDomainNames = [
    "vault.${hostName}.${suffix}"
  ];
  services.nginx.virtualHosts = {
    "${vaultwardenAddress}" = {
      useACMEHost = "${hostName}.${suffix}";
      acmeRoot = "/var/lib/acme/challenge-${hostName}-${suffix}";
      forceSSL = true;
      locations."/" = {
        proxyPass = "http://${vaultCfg.ROCKET_ADDRESS}:${toString vaultCfg.ROCKET_PORT}";
      };
    };
  };
  sops = {
    secrets."vaultwarden/admin" = {};
    templates."vaultwarden-environment".content = ''
      ADMIN_TOKEN="${config.sops.placeholder."vaultwarden/admin"}"
      DATABASE_URL="postgresql://vaultwarden:${config.sops.placeholder."postgresql/vaultwarden"}@localhost/vaultwarden?sslmode=disable"
    '';
  };
  services.vaultwarden = {
    enable = true;
    dbBackend = "postgresql";
    backupDir = null;
    environmentFile = config.sops.templates."vaultwarden-environment".path;
    config = {
      DOMAIN = "https://${vaultwardenAddress}";
      LOG_LEVEL = "error";
      SIGNUPS_ALLOWED = false;
      ROCKET_ADDRESS = "127.0.0.1";
      ROCKET_PORT = 8000;
      ROCKET_LOG = "critical";
    };
  };
}
