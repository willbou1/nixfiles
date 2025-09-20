{config, ...}: let
  reserved = toString (config.home.gapSize / 2 + config.home.ewwHeight);
in {
  wayland.windowManager.hyprland.settings = {
    monitor = [
      (
        if config.home.verticalDisplays
        then "HDMI-A-1,addreserved,${reserved},0,0,0"
        else "HDMI-A-1,addreserved,0,${reserved},0,0"
      )
      "DP-1,addreserved,0,${reserved},0,0"
    ];
    workspace = [
      "DP-1,1"
      "HDMI-A-1,2"
      "2, monitor:HDMI-A-1"
      "1, monitor:DP-1"
      "5, monitor:DP-1"
      "6, monitor:DP-1"
      "7, monitor:DP-1"
      "9, monitor:DP-1"
    ];
    exec-once = [
      "~/.config/eww/launch.sh"
    ];
    bind = [
      "$mod SHIFT,W,exec,looking-glass-client -f /dev/kvmfr1"
      "$mod,Q,exec,${config.programs.swaylock.package}/bin/swaylock --grace 0"
    ];
  };
}
