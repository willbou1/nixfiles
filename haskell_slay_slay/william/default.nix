{lib, ...}:
with lib; {
  imports = mine.autoInclude ./. [];

  home.persistence."/persist/home/william".directories = [
    "priv"
  ];

  stylix.image = ../../resources/wallpapers/eva_night.jpg;

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
        y = 0;
        hScale = 2;
        vScale = 2;
      }
    ];
  };
}
