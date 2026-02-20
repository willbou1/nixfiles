{
  lib,
  config,
  pkgs,
  ...
}:
with lib; {
  imports = (mine.autoInclude ./. [
    ./deprecated
  ]) ++ [../../school.nix];

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
    home.persistence."/persist" = {
      directories = [
        ".mozilla/firefox"
        ".zen"
        ".config/libreoffice"
        ".config/Nextcloud"
      ];
      files = [
        ".config/deluge/hostlist.conf"
        ".config/deluge/gtk3ui.conf"
      ];
    };

    home.file."media".source =
      config.lib.file.mkOutOfStoreSymlink "/run/media/william";

    home.packages = with pkgs; [
      dconf
      playerctl
      hyprpaper
      swww

      wally-cli

      libreoffice-fresh
      #gimp
      helvum
      steam
      bitwig-studio
      qalculate-gtk

      zen-browser

      texlive.combined.scheme-full
      texlab
    ];

    services = {
      nextcloud-client.enable = true;
      udiskie = {
        enable = true;
        tray = "never";
      };
    };
  };
}
