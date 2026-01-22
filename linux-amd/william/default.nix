{
  config,
  lib,
  ...
}:
with lib; {
  imports = mine.autoInclude ./. [];

  options.home = {
    verticalDisplays = mkOption {
      type = types.bool;
    };
  };

  config = {
    home = {
      verticalDisplays = true;
      gapSize = 20;
      borderSize = 5;
      monitors = with config.home; [
        {
          # Philips ultrawide
          wlrName = "HDMI-A-1";
          xrandrName = "HDMI-A-0";
          width = 3440;
          height = 1440;
          rate = 100;
          x =
            if verticalDisplays
            then 0
            else 3440;
          y = 0;
          hScale = 1;
          vScale = 1;
        }
        {
          # Alienware ultrawide
          wlrName = "DP-1";
          xrandrName = "DisplayPort-0";
          width = 3440;
          height = 1440;
          rate = 180;
          x = 0;
          y =
            if verticalDisplays
            then 1440
            else 0;
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
  };
}
