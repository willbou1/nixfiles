{
  pkgs,
  lib,
  ...
}:
with lib; {
  imports = mine.autoInclude ./. [];

  home = {
    gapSize = 20;
    borderSize = 5;
    monitors = [
      {
        wlrName = "HDMI-A-1";
        xrandrName = "HDMI-A-0";
        width = 3440;
        height = 1440;
        rate = 100;
        x = 0;
        y = 0;
        hScale = 1;
        vScale = 1;
      }
      {
        wlrName = "DP-1";
        xrandrName = "DisplayPort-0";
        width = 3440;
        height = 1440;
        rate = 100;
        x = 0;
        y = 1440;
        hScale = 1;
        vScale = 1;
      }
    ];
  };

  # little adjustments
  programs.qutebrowser.settings.zoom.default = mkForce "175%";
  services.dunst.settings.global = {
    monitor = mkForce 1;
    origin = mkForce "top-right";
  };
}
