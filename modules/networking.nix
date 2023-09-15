{ inputs, lib, config, pkgs, ... }: let
expressvpnServers = servers: builtins.listToAttrs(map (s: {
    name = s.name;
    value = {
        config = ''config /etc/openvpn/expressvpn/my_expressvpn_${lib.strings.stringAsChars (c: if c == " " then "_" else c) s.fullName}_udp.ovpn
        auth-user-pass /etc/openvpn/expressvpn/creds.txt'';
        autoStart = s.autoStart;
    };
}) servers);
in {
	imports = [
		inputs.hosts.nixosModule
	];
	networking = {
		hostName = "haskell_slay_slay";
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
		deluge = {
            enable = true;
            config = {
                download_location = "/srv/torrents";
                incoming_interface = "tun0";
                outcoming_interface = "tun0";
            };
        };
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
        openvpn.servers = expressvpnServers [
            { name = "canada"; fullName = "canada - toronto - 2"; autoStart = true; }
            { name = "korea"; fullName = "south korea - 2"; autoStart = false; }
            { name = "japan"; fullName = "japan - tokyo"; autoStart = false; }
        ];
	};
}
