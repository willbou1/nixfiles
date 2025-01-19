{config, ...}: let
  hostName = config.networking.hostName;
  suffix = config.networking.suffix;
in {
  security.acme = {
    acceptTerms = true;
    defaults.email = "mr.nobody.0000000000@protonmail.ch";
    certs."${hostName}.${suffix}" = {
      webroot = "/var/lib/acme/challenge-${hostName}-${suffix}";
      group = "nginx";
      extraDomainNames = ["pgp.${hostName}.${suffix}"];
    };
  };
  networking.firewall.allowedTCPPorts = [80 443];
  services.nginx = {
    enable = true;
    logError = "stderr";
    recommendedGzipSettings = true;
    appendHttpConfig = ''
      sendfile        on;

      keepalive_timeout  65;
    '';
    virtualHosts = {
      "pgp.${hostName}.${suffix}" = {
        useACMEHost = "${hostName}.${suffix}";
        acmeRoot = "/var/lib/acme/challenge-${hostName}-${suffix}";
        forceSSL = true;
        root = "/srv/pgp";
      };
    };
  };
}
