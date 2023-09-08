{ inputs, lib, config, pkgs, ... }:

{
	imports = [
		inputs.hosts.nixosModule
	];
	networking = {
		hostName = "linux-laptop";
		networkmanager = {
            enable = true;
            connectionConfig = {
                "wifi.cloned-mac-address" = lib.mkForce "random";
                "ethernet.cloned-mac-address" = lib.mkForce "random";
                "connection.stable-id" = "id=\${CONNECTION}/\${BOOT}";
            };
            extraConfig = ''
                [devices]
                wifi.scan-rand-mac-address=yes
            '';
        };
		useDHCP = lib.mkDefault true;
		
		stevenBlackHosts = {
			enable = true;
			blockFakenews = true;
		};

		firewall = {
            enable = true;
            allowPing = false;
            # allowedTCPPorts = [ ... ];
            # allowedUDPPorts = [ ... ];
        };
	};
	services = {
		expressvpn.enable = true;
		deluge.enable = true;
        avahi = {
            enable = true;
            nssmdns = true;
        };
		openssh = {
			enable = true;
			settings = {
				PermitRootLogin = "no";
				AllowUsers = "william";
			};
		};
	};
}
