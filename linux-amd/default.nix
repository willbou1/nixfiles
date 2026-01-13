{
  pkgs,
  lib,
  ...
}:
with lib; {
  imports = mine.autoInclude ./. [
    ./william
  ];

  wallpaper = ../resources/wallpapers/eva_abstract.png;

  environment.shellAliases = {
    "nr" = "sudo nixos-rebuild --impure --show-trace --flake '/etc/nixos?submodules=1#linux-amd' switch &| nom";
  };

  programs.i3lock = {
    enable = true;
    package = pkgs.i3lock-color;
  };

  services.udev.extraRules = ''
    KERNEL=="i2c-[0-9]*", GROUP="i2c", MODE="0660"
  '';
  users = {
    groups.i2c = {};
    users.william.extraGroups = ["i2c"];
  };

  # 20 cores used total
  nix.settings = {
    max-jobs = 5;
    cores = 4;
  };

  # linux-amd is in my room so I'm not that concerned
  security.sudo.extraConfig = ''
    Defaults timestamp_timeout=20
  '';
}
