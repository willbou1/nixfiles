{
  lib,
  config,
  pkgs,
  ...
}:
with lib; {
  #new version
  imports = mine.autoInclude ./. [
    ./deprecated
  ];

  options.home = {
    terminal = mkOption {
      type = types.str;
    };
    monitors = mkOption {
      type = with types; listOf (attrsOf (oneOf [float int str]));
    };
    gapSize = mkOption {
      type = types.int;
    };
    borderSize = mkOption {
      type = types.int;
    };
  };
  config = {
    home.persistence."/persist/home/william" = {
      directories = [
        ".mozilla/firefox"
        ".config/libreoffice"
      ];
      files = [
        ".config/deluge/hostlist.conf"
        ".config/deluge/gtk3ui.conf"
      ];
    };
    home.file."media".source =
      config.lib.file.mkOutOfStoreSymlink "/run/media/william";

    home.packages = with pkgs; [
      libreoffice-fresh
      gimp
      playerctl
      hyprpaper
      swww
      dconf
      helvum
      steam
      wally-cli
    ];

    programs = {
      firefox.enable = true;
    };

    services = {
      udiskie = {
        enable = true;
        tray = "never";
      };
      easyeffects.enable = true;
    };

    # default to aptx_ll for my bluetooth headphones
    home.file.".local/state/wireplumber/default-profile".text = ''
      [default-profile]
      bluez_card.00_1B_66_C0_52_BD=a2dp-sink-aptx_ll
    '';
  };
}
