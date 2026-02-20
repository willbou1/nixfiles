{lib, ...}:
with lib; {
  imports = mine.autoInclude ./. [
    ./william
  ];

  wallpaper = ../resources/wallpapers/eva_abstract.png;

  home-manager.users.william = import ./william;

  environment.persistence."/persist".directories = [
    "/srv"
  ];
  networking = {
    hostName = "haskell_slay_slay";
    ip = "10.0.0.161";
    subnet = "10.0.0.0";
    subnetLength = 24;
    gateway = "10.0.0.1";
    mainInterface = "wlp0s20f3";
  };
  environment.shellAliases = {
    "nr" = "sudo nixos-rebuild --impure --keep-going --show-trace --flake '/etc/nixos#haskell_slay_slay' switch &| nom";
    "no" = "nixos-options --flake '/etc/nixos#haskell_slay_slay'";
  };
  services.deluge.config.download_location = "/srv/torrents";

  # 16 cores used total
  nix.settings = {
    max-jobs = 4;
    cores = 4;
  };
}
