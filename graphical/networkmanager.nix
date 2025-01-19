{
  lib,
  config,
  ...
}: let
  wifiAddress = with config.networking; "${ip}/${toString subnetLength},${gateway}";
in {
  sops = {
    secrets = {
      "wifi/maman" = {};
      "wifi/papa" = {};
      "wifi/udes" = {};
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
      address1=${wifiAddress}
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
      address1=${wifiAddress}
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
  };
  environment = {
    persistence."/persist".directories = [
      "/cat_installer"
    ];
    etc."NetworkManager/system-connections/maman.nmconnection".source = config.sops.templates."maman.nmconnection".path;
    etc."NetworkManager/system-connections/papa.nmconnection".source = config.sops.templates."papa.nmconnection".path;
    etc."NetworkManager/system-connections/udes.nmconnection".source = config.sops.templates."udes.nmconnection".path;
  };
  networking = {
    networkmanager = {
      enable = true;
      connectionConfig = {
        "wifi.cloned-mac-address" = lib.mkForce "random";
        "ethernet.cloned-mac-address" = lib.mkForce "random";
        "connection.stable-id" = "id=\${CONNECTION}/\${BOOT}";
      };
      settings.devices."wifi.scan-rand-mac-address" = true;
    };
  };
}
