{lib, ...}:
with lib; {
  imports = lib.mine.autoInclude ./. [./in_progress ./william];

  options.networking = {
    suffix = mkOption {
      type = types.str;
    };
  };

  config = {
    users.users.william.openssh.authorizedKeys.keys = ["ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICJ2b2UtfnyyWsNKR96dUK6l1iVaEUc1uTEf8X8VBZeC willbou2@gmail.com"];
    services.openssh = {
      enable = true;
      # only the first one will be used in the initrd
      ports = [2223];
      startWhenNeeded = true;
    };
    networking = {
      hostName = "ourmiraculous";
      suffix = "com";
      nameservers = ["9.9.9.9" "149.112.112.112"];
      firewall.allowedTCPPorts = [2223];
      usePredictableInterfaceNames = false;
      useDHCP = false;
    };
    systemd.network = {
      enable = true;
      networks."eth0" = {
        name = "eth0";
        address = ["185.165.171.79/25"];
        gateway = ["185.165.171.1"];
      };
    };
    environment.shellAliases = {
      "nr" = "sudo nixos-rebuild --show-trace --flake '/etc/nixos?submodules=1#vps' switch &| nom";
    };
    nixpkgs.config.permittedInsecurePackages = [
      "olm-3.2.16"
    ];
  };
}
