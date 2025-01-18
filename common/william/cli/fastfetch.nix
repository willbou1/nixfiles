{
  pkgs,
  lib,
  ...
}:
with lib; let
  fastfetch = "${pkgs.fastfetch}/bin/fastfetch";
in {
  programs.fish.functions.fish_greeting = ''
    if string match -q $PRIVATE "private"
        tty | grep tty > /dev/null && ${fastfetch} || ${fastfetch} -l (random choice ~/.config/fastfetch/images/*.png)
    else
        tty | grep tty > /dev/null && ${fastfetch} || ${fastfetch} -l ~/.config/fastfetch/images/image0.png
    end
  '';

  programs.fastfetch = {
    enable = true;
    settings = {
      logo = {
        type = "kitty-direct";
        width = 20;
        height = 10;
      };
      modules = [
        "os"
        "battery"
        "kernel"
        "cpu"
        "gpu"
        "memory"
        "swap"
        {
          type = "disk";
          key = "Disk";
          showSubvolumes = true;
          folders = "/:/home/william";
          format = "{9} - {3}";
        }
        "btrfs"
      ];
    };
  };

  xdg.configFile = {
    "fastfetch/images".source = ../../../resources/splash/images;
  };
}
