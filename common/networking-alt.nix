{ inputs, lib, config, pkgs, ... }:
with lib;
with builtins;
let
notnft = config.notnft;
evalRuleset = val: (lib.modules.evalModules {
    modules = [ {
      config.val = val;
      options.val = lib.mkOption { type = notnft.types.ruleset; };
    } ];
  }).config.val;
expressvpnServers = servers: builtins.listToAttrs(map (s: {
    name = s.name;
    value = {
        config = ''config /etc/openvpn/expressvpn/my_expressvpn_${strings.stringAsChars (c: if c == " " then "_" else c) s.fullName}_udp.ovpn
        auth-user-pass ${config.sops.templates."expressvpn_creds.txt".path}'';
        autoStart = s.autoStart;
    };
}) servers);
networking = config.networking;
in {
	options.networking = {
        ip = mkOption {
            type = types.str;
        };
        subnetLength = mkOption {
            type = types.int;
        };
        gateway = mkOption {
            type = types.str;
        };
        subnet = mkOption {
            type = types.str;
        };
        mainInterface = mkOption {
            type = types.str;
        };
    };
    config = {
        sops = {
            secrets = {
                "expressvpn/user" = {};
                "expressvpn/password" = {};
            };
            templates."expressvpn_creds.txt".content = ''
                ${config.sops.placeholder."expressvpn/user"}
                ${config.sops.placeholder."expressvpn/password"}
            '';
        };
        environment = {
            etc."openvpn/expressvpn".source = ../resources/expressvpn;
        };

        environment.etc."test".text = toJSON (evalRuleset (with notnft.dsl; with payload; ruleset {
            filter = add table { family = f: f.inet; } {
                upnp = add set {
                    type = f: with f; [ ipv4_addr inet_proto inet_service ];
                    timeout = 3;
                    size = 65536;
                };

                output = add chain  {
                    type = f: f.filter; hook = f: f.output;
                    prio = f: f.filter; policy = f: f.accept; }
                    [(is.eq ip.daddr (cidr "239.255.255.250" 32))
                        (is.eq ip.protocol (f: f.udp))
                        (is.eq udp.dport 1900)
                        (set.add "@upnp" (elem {} [ ip.saddr ip.protocol udp.sport ]))];

                input = add chain {
                    type = f: f.filter; hook = f: f.input;
                    prio = f: f.filter; policy = f: f.drop; }
                    [(is.eq ip.daddr "@upnp")
                        (is.eq ip.protocol "@upnp")
                        (is.eq udp.dport "@upnp")
                        accept];
            };
        }));

        networking = {
            useDHCP = lib.mkDefault true;
            
            stevenBlackHosts = {
                enable = true;
                blockFakenews = true;
            };

            nftables.enable = true;

            firewall = {
                enable = true;
                allowPing = false;
                # allowedTCPPorts = [ ... ];
                # allowedUDPPorts = [ ... ];
            };

            iproute2 = {
                enable = true;
                rttablesExtraConfig = ''
                '';
            };

            #hostnames of all the computers I use on my LANs
            extraHosts = ''
                10.0.0.160 linux-amd
                10.0.0.161 haskell_slay_slay
            '';
        };
        services = {
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
