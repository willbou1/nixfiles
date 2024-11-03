{ lib, ... }:

{
    imports = lib.mine.autoInclude ./. [ ./in_progress ./william ];

    networking = {
        hostName = "srv631436";
    };
	        users.extraUsers.root.openssh.authorizedKeys.keys = [ "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICJ2b2UtfnyyWsNKR96dUK6l1iVaEUc1uTEf8X8VBZeC willbou2@gmail.com" ];
        services.openssh = {
            enable = true;
            startWhenNeeded = true;
        };
        networking = {
            firewall.allowedTCPPorts = [ 22 ];
            usePredictableInterfaceNames = false;
            useDHCP = false;
        };
        systemd.network.enable = true;
        environment.etc."systemd/network/eth0.network".text = ''
            [Match]
            Name = eth0
            [Network]
            Address = 92.112.181.70/24
            Gateway = 92.112.181.254
        '';

}
