{config, ...}: let
  hostName = config.networking.hostName;
  suffix = config.networking.suffix;
  turnAddress = "turn.${hostName}.${suffix}";
  matrixAddress = "${hostName}.${suffix}";
  coturn = config.services.coturn;
  ports = [coturn.listening-port coturn.alt-listening-port];
  tlsPorts = [coturn.tls-listening-port coturn.alt-tls-listening-port];
in {
  sops.secrets."coturn/secret".owner = "turnserver";

  services.coturn = {
    enable = true;
    secure-stun = true;
    no-cli = true;
    no-tcp-relay = true;
    min-port = 49000;
    max-port = 50000;
    use-auth-secret = true;
    static-auth-secret-file = config.sops.secrets."coturn/secret".path;
    realm = matrixAddress;
    cert = "${config.security.acme.certs.${turnAddress}.directory}/full.pem";
    pkey = "${config.security.acme.certs.${turnAddress}.directory}/key.pem";
    extraConfig = ''
      no-multicast-peers
      no-stdout-log
      simple-log
      log-file=/dev/null
    '';
  };

  networking.firewall = let
    range = {
      from = coturn.min-port;
      to = coturn.max-port;
    };
  in {
    allowedUDPPortRanges = [range];
    allowedUDPPorts = tlsPorts;
    allowedTCPPorts = tlsPorts;
  };

  security.acme.certs.${turnAddress} = {
    webroot = "/var/lib/acme/challenge-turn-${hostName}-${suffix}";
    postRun = "systemctl restart coturn.service";
    group = "turnserver-acme";
  };
  users.groups."turnserver-acme".members = ["turnserver" "nginx"];
  services.nginx.virtualHosts = {
    ${turnAddress} = {
      useACMEHost = turnAddress;
      acmeRoot = "/var/lib/acme/challenge-turn-${hostName}-${suffix}";
      forceSSL = true;
    };
  };

  services.matrix-synapse.settings = {
    turn_uris =
      (map (p: "turns:${turnAddress}:${toString p}?transport=tcp") tlsPorts)
      ++ (map (p: "turns:${turnAddress}:${toString p}?transport=udp") tlsPorts);
    turn_user_lifetime = "6h";
  };
}
