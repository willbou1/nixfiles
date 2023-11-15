{ pkgs, lib, config, inputs, ... }: let
    reserved = toString (config.home.gapSize / 2 + config.home.ewwHeight);
in {
    wayland.windowManager.hyprland.settings = {
        monitor = [
            "HDMI-A-1,addreserved,${reserved},0,0,0"
            "DP-1,addreserved,${reserved},0,0,0"
        ];
        workspace = [
            "HDMI-A-1,1"
            "DP-1,2"
            "2, monitor:DP-1"
            "1, monitor:HDMI-A-1"
            "5, monitor:HDMI-A-1"
            "6, monitor:HDMI-A-1"
            "7, monitor:HDMI-A-1"
            "9, monitor:HDMI-A-1"
        ];
        exec-once = [
            "~/.config/eww/launch.sh"
        ];
        bind = [
            "$mod SHIFT,W,exec,looking-glass-client -f /dev/shm/looking-glass2"
        ];
    };
}
