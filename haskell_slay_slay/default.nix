{ lib, ... }:
with lib;
{
    imports = mine.autoInclude ./. [
        ./william
    ];

    environment.persistence."/persist".directories = [
        "/srv/torrents"
    ];
    networking = {
        hostName = "haskell_slay_slay";
        ip = "10.0.0.161";
        subnet = "10.0.0.0";
        subnetLength = 24;
        gateway = "10.0.0.1";
        mainInterface = "wlp0s20f3";
    };
    environment.shellAliases = {
        "nr" = "sudo nixos-rebuild --flake /etc/nixos?submodules=1#haskell_slay_slay switch";
    };
    services.deluge.config.download_location = "/srv/torrents";
}
