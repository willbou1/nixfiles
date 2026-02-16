{
  lib,
  pkgs,
  ...
}:
with lib; {
  imports = mine.autoInclude ./. [
    ./specialisations
    ./william
    ./networking-alt.nix
  ];

  home-manager.users.william = import ./william;

  stylix.targets.grub.useWallpaper = true;

  environment = {
    systemPackages = with pkgs; [
      exfat
      zsa-udev-rules
    ];
    shellAliases = {
      "b" = "bluetoothctl";
      "virsh" = "virsh -c qemu:///system";
      "vs" = "virsh";
    };
    sessionVariables.NIXOS_OZONE_WL = "1";
  };

  programs.steam.enable = true;

  nixpkgs = {
    config.joypixels.acceptLicense = true;
  };

  xdg = {
    autostart.enable = true;
    portal = {
      enable = true;
      extraPortals = [
        pkgs.xdg-desktop-portal
        pkgs.xdg-desktop-portal-hyprland
        pkgs.xdg-desktop-portal-gtk
      ];
      config.common.default = "*";
    };
  };

  hardware.sane = {
    enable = true;
    extraBackends = [pkgs.sane-airscan];
  };

  services = {
    udisks2.enable = true;
    xserver = {
      enable = true;
      xkb = {
        layout = "ca";
        variant = "multix";
      };
      displayManager.startx.enable = true;
    };
  };
}
