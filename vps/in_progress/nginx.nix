{ pkgs, config, ... }:
let
hostName = config.networking.hostName;
in {
    security.acme = {
        acceptTerms = true;
        defaults.email = "mr.nobody.0000000000@protonmail.ch";
    };
    services.nginx = {
        httpConfig = ''
            include mime.types;
            default_type application/octet-stream;

            error_log    off;
            access_log   off;

            sendfile        on;
            #tcp_nopush     on;

            #keepalive_timeout  0;
            keepalive_timeout  65;

            gzip  on;
        '';
        virtualHosts = {
            "PGP" = {
                serverName = "pgp.${hostName}";
                enableACME = true;
                forceSSL = true;
                locations."/" = {
                    root = "/srv/pgp";
                };
            };
        };
    };

}
