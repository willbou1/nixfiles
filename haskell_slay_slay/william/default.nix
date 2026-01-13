{lib, ...}:
with lib; {
  imports = mine.autoInclude ./. [];

  home.persistence."/persist/home/william".directories = [
    "priv"
  ];

  home = {
    gapSize = 8;
    borderSize = 3;
    monitors = [
      {
        wlrName = "eDP-1";
        xrandrName = "eDP-1";
        width = 3840;
        height = 2400;
        rate = 60;
        x = 0;
        y = 960;
        hScale = 2;
        vScale = 2;
      }
      {
        wlrName = "DP-3";
        xrandrName = "DP-3";
        width = 1920;
        height = 1080;
        rate = 120;
        x = 1920;
        y = 0;
        hScale = 1;
        vScale = 1;
      }
      {
        wlrName = "DP-4";
        xrandrName = "DP-4";
        width = 1920;
        height = 1080;
        rate = 120;
        x = 1920;
        y = 1080;
        hScale = 1;
        vScale = 1;
      }
    ];
  };
}
