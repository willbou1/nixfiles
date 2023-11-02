{ pkgs, lib, config, inputs, ... }:
{
    wayland.windowManager.hyprland.settings = {
        monitor = [
            "HDMI-A-1,3440x1440@100,0x0,1,bitdepth,10"
            "HDMI-A-1,addreserved,80,0,0,0"
            "DP-1,3440x1440@100,3440x0,1,bitdepth,10"
            "DP-1,addreserved,80,0,0,0"
            ",disable"
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
            "~/.config/eww/launch"
        ];
        bind = [
            "$mod SHIFT,W,exec,looking-glass-client -f /dev/shm/looking-glass2"
        ];
    };
}
