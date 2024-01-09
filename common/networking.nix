{ inputs, lib, config, pkgs, ... }: let
expressvpnServers = servers: builtins.listToAttrs(map (s: {
    name = s.name;
    value = {
        config = ''config /etc/openvpn/expressvpn/my_expressvpn_${lib.strings.stringAsChars (c: if c == " " then "_" else c) s.fullName}_udp.ovpn
        auth-user-pass ${config.sops.templates."expressvpn_creds.txt".path}'';
        autoStart = s.autoStart;
    };
}) servers);
in {
	options.networking.networkmanager.wifiAddress = lib.mkOption {
		type = lib.types.str;
	};
	config = {
    sops = {
        secrets = {
            "wifi/maman" = {};
            "wifi/papa" = {};
            "wifi/udes" = {};
            "expressvpn/user" = {};
            "expressvpn/password" = {};
        };
        templates."maman.nmconnection".content = ''
[connection]
id=maman
uuid=35fe8ef3-1770-4e86-a1c4-3e381153595d
type=wifi

[wifi]
mode=infrastructure
ssid=Pacha

[wifi-security]
auth-alg=open
key-mgmt=wpa-psk
psk=${config.sops.placeholder."wifi/maman"}

[ipv4]
address1=${config.networking.networkmanager.wifiAddress}
method=manual

[ipv6]
addr-gen-mode=default
method=auto

[proxy]
        '';
        templates."papa.nmconnection".content = ''
[connection]
id=papa
uuid=a13cea5f-cd1c-4b2e-a180-829a3844eae4
type=wifi
timestamp=1694489603

[wifi]
mode=infrastructure
ssid=Casabouellette12

[wifi-security]
auth-alg=open
key-mgmt=wpa-psk
psk=${config.sops.placeholder."wifi/papa"}

[ipv4]
address1=${config.networking.networkmanager.wifiAddress}
dns=9.9.9.9;
method=manual

[ipv6]
addr-gen-mode=default
method=auto

[proxy]
        '';
        templates."udes.nmconnection".content = ''
[connection]
id=udes
uuid=808629a6-b065-44a5-9d7d-50145c3d616e
type=wifi
timestamp=1694015437

[wifi]
mode=infrastructure
ssid=eduroam

[wifi-security]
group=ccmp;tkip;
key-mgmt=wpa-eap
pairwise=ccmp;

[802-1x]
altsubject-matches=DNS:radius.usherbrooke.ca;
anonymous-identity=anonymous657357@usherbrooke.ca
ca-cert=/cat_installer/ca.pem
eap=peap;
identity=bouw1002
password=${config.sops.placeholder."wifi/udes"}
phase2-auth=mschapv2

[ipv4]
method=auto

[ipv6]
addr-gen-mode=default
method=auto

[proxy]
        '';
        templates."expressvpn_creds.txt".content = ''
${config.sops.placeholder."expressvpn/user"}
${config.sops.placeholder."expressvpn/password"}
        '';
    };
    environment = {
        persistence."/persist".directories = [
            "/cat_installer"
        ];
        etc."NetworkManager/system-connections/maman.nmconnection".source = config.sops.templates."maman.nmconnection".path;
        etc."NetworkManager/system-connections/papa.nmconnection".source = config.sops.templates."papa.nmconnection".path;
        etc."NetworkManager/system-connections/udes.nmconnection".source = config.sops.templates."udes.nmconnection".path;
        etc."openvpn/expressvpn".source = ../resources/expressvpn;
    };
	networking = {
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
        avahi = {
            enable = true;
            nssmdns4 = true;
        };
		openssh = {
			enable = true;
			settings = {
				PermitRootLogin = "no";
				AllowUsers =  [ "william" ];
			};
		};
        openvpn.servers = expressvpnServers [
            { name = "canada"; fullName = "canada - toronto - 2"; autoStart = true; }
            { name = "korea"; fullName = "south korea - 2"; autoStart = false; }
            { name = "japan"; fullName = "japan - tokyo"; autoStart = false; }
        ];
	};
};
}
