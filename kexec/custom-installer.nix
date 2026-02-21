import ./kexec-installer.nix {
  extraConfig = {pkgs, ...}: {
    users.extraUsers.root.openssh.authorizedKeys.keys = ["ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICJ2b2UtfnyyWsNKR96dUK6l1iVaEUc1uTEf8X8VBZeC willbou2@gmail.com"];
    services.openssh = {
      enable = true;
      startWhenNeeded = true;
    };
    networking = {
      firewall.allowedTCPPorts = [22];
      usePredictableInterfaceNames = false;
      useDHCP = false;
    };
    systemd.network.enable = true;
    environment.etc."systemd/network/eth0.network".text = ''
      [Match]
      Name = eth0
      [Network]
      Address = 185.165.171.79/25
      Gateway = 185.165.171.1
    '';
    environment.systemPackages = with pkgs; [
      neovim
      git
      rsync
    ];
    nix.settings = {
      experimental-features = ["nix-command" "flakes"];
    };
  };
}
