{ config, ... }:

{
    sops.secrets = {
        "shadowsocks/password" = {};
    };

    services.shadowsocks = {
        enable = true;
        passwordFile = config.sops.secrets."shadowsocks/password".path;
    };
}
