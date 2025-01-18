{
  inputs,
  lib,
  config,
  pkgs,
  ...
}:
with lib;
with builtins; let
  #googleIPsPkg = fetchurl {
  #  url = "https://www.gstatic.com/ipranges/goog.json";
  #  sha256 = "17qrx00i26aaf09xszqp0di0qg1m20v2wn12mwjgwyfjzkhvypqp";
  #};
  #cidrToMask = p: let
  #  ones = genList (_: "1") p;
  #  zeros = genList (_: "0") (32 - p);
  #  bin = ones ++ zeros;
  #  dec = foldl' (a: b: a * 2 + b) 0;
  #  group = i: toString (dec (sublist (i * 8) 8 bin)); in
  #  "${group 0}.${group 1}.${group 2}.${group 3}";
  #googleIPsJSON = fromJSON (readFile googleIPsPkg);
  #googleIPLines = map
  #  (p: let ns = attrNames p;
  #    ip = i: head (split "/" i);
  #    prefix = pr: tail (split "/" pr);
  #    mask = cidrToMask prefix; in
  #    if elem "ipv4Prefix" ns then
  #      "route ${ip p.ipv4Prefix} ${mask p.ipv4Prefix} net_gateway"
  #    else if elem "ipv6Prefix" ns then
  #      "route ${ip p.ipv6Prefix} ${mask p.ipv6Prefix} net_gateway"
  #    else
  #      "")
  #  googleIPsJSON.prefixes;
  expressvpnServers = servers:
    builtins.listToAttrs (map (s: {
        name = s.name;
        value = {
          config = ''
            config /etc/openvpn/expressvpn/my_expressvpn_${strings.stringAsChars (c:
              if c == " "
              then "_"
              else c)
            s.fullName}_udp.ovpn
            auth-user-pass ${config.sops.templates."expressvpn_creds.txt".path}
          '';
          updateResolvConf = true;
          autoStart = s.autoStart;
        };
      })
      servers);
  networking = config.networking;
  openPorts = protocol: ports:
    concatStringsSep "\n"
    (map (p: "upnpc -a ${networking.ip} ${toString p} ${toString p} ${protocol}") ports);
  openPortRanges = protocol: ranges:
    concatStringsSep "\n"
    (map (r: ''
        for p in {${toString r.from}..${toString r.to}}; do
            upnpc -a ${networking.ip} $p $p ${protocol}
        done
      '')
      ranges);
  upnpcScript = with networking.firewall;
    concatStringsSep "\n" [
      "# Opening TCP ports"
      (openPorts "tcp" allowedTCPPorts)
      "# Opening UDP ports"
      (openPorts "udp" allowedUDPPorts)
      "# Opening TCP port ranges"
      (openPortRanges "tcp" allowedTCPPortRanges)
      "# Opening UDP port ranges"
      (openPortRanges "udp" allowedUDPPortRanges)
    ];
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

    networking = {
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
        extraPackages = with pkgs; [
          iproute2
          ipset
        ];
        extraCommands = ''
          # Clear my chains too
          ip46tables -F OUTPUT 2> /dev/null || true
          ip46tables -X OUTPUT 2> /dev/null || true

          # Allow UPNP traffic
          ipset create upnp hash:ip,port family inet hashsize 1024 maxelem 65536 timeout 3 bucketsize 12 initval 0x49fc21d8 || true
          iptables -I OUTPUT -d 239.255.255.250/32 -p udp -m udp --dport 1900 -j SET --add-set upnp src,src --exist
          iptables -I nixos-fw -p udp -m set --match-set upnp dst,dst -j ACCEPT

          # Prevent server reply packets from going through the VPN
          ip rule flush table 128 || true
          ip route flush table 128 || true
          ip rule add from ${networking.ip} table 128 || true
          ip route add table 128 to ${networking.subnet}/${toString networking.subnetLength} dev ${networking.mainInterface} || true
          ip route add table 128 default via ${networking.gateway} || true

          # Prevent UPNP broadcast packets from going through the VPN
          ip rule flush table 127 || true
          ip route flush table 127 || true
          ip rule add to 239.255.255.250/32 dport 1900 table 127 || true
          ip route add table 127 to ${networking.subnet}/${toString networking.subnetLength} dev ${networking.mainInterface} || true
          ip route add table 127 default via ${networking.gateway} || true
        '';
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
          AllowUsers = ["william"];
        };
      };
      openvpn.servers = expressvpnServers [
        {
          name = "canada-toronto";
          fullName = "canada - toronto";
          autoStart = false;
        }
        {
          name = "canada-montreal";
          fullName = "canada - montreal";
          autoStart = false;
        }
        {
          name = "korea";
          fullName = "south korea - 2";
          autoStart = false;
        }
        {
          name = "japan";
          fullName = "japan - tokyo";
          autoStart = false;
        }
        {
          name = "romania";
          fullName = "romania";
          autoStart = false;
        }
        {
          name = "taiwan";
          fullName = "taiwan - 3";
          autoStart = false;
        }
      ];
    };

    # Open ports automatically using UPNPC
    systemd = {
      timers."upnpc" = {
        wantedBy = ["timers.target"];
        timerConfig = {
          OnBootSec = "5m";
          OnUnitActiveSec = "12h";
          Unit = "upnpc.service";
        };
      };
      services."upnpc" = {
        script = upnpcScript;
        path = with pkgs; [miniupnpc];
        serviceConfig = {
          Type = "oneshot";
          User = "root";
        };
      };
    };
  };
}
