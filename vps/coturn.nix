{
  config,
  pkgs,
  ...
}: let
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
      denied-peer-ip=0.0.0.0-0.255.255.255
      denied-peer-ip=10.0.0.0-10.255.255.255
      denied-peer-ip=100.64.0.0-100.127.255.255
      denied-peer-ip=127.0.0.0-127.255.255.255
      denied-peer-ip=169.254.0.0-169.254.255.255
      denied-peer-ip=172.16.0.0-172.31.255.255
      denied-peer-ip=192.0.0.0-192.0.0.255
      denied-peer-ip=192.0.2.0-192.0.2.255
      denied-peer-ip=192.88.99.0-192.88.99.255
      denied-peer-ip=192.168.0.0-192.168.255.255
      denied-peer-ip=198.18.0.0-198.19.255.255
      denied-peer-ip=198.51.100.0-198.51.100.255
      denied-peer-ip=203.0.113.0-203.0.113.255
      denied-peer-ip=240.0.0.0-255.255.255.255
      denied-peer-ip=::1
      denied-peer-ip=64:ff9b::-64:ff9b::ffff:ffff
      denied-peer-ip=::ffff:0.0.0.0-::ffff:255.255.255.255
      denied-peer-ip=100::-100::ffff:ffff:ffff:ffff
      denied-peer-ip=2001::-2001:1ff:ffff:ffff:ffff:ffff:ffff:ffff
      denied-peer-ip=2002::-2002:ffff:ffff:ffff:ffff:ffff:ffff:ffff
      denied-peer-ip=fc00::-fdff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
      denied-peer-ip=fe80::-febf:ffff:ffff:ffff:ffff:ffff:ffff:ffff
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
