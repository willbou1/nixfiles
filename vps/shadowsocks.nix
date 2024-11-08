{ config, ... }:

{
    sops.secrets = {
        "shadowsocks/password" = {};
    };

    networking.firewall.allowedTCPPorts = [ config.services.shadowsocks.port ];
    services.shadowsocks = {
        enable = true;
        passwordFile = config.sops.secrets."shadowsocks/password".path;
    };
}
