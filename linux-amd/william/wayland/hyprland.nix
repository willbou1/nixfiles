{ pkgs, lib, config, inputs, ... }: let
    reserved = toString (config.home.gapSize / 2 + config.home.ewwHeight);
in {
    wayland.windowManager.hyprland.settings = {
        monitor = [
            "HDMI-A-1,addreserved,${reserved},0,0,0"
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
            "sleep 2; ${pkgs.swww}/bin/swww img ${config.stylix.image}"
        ];
        bind = [
            "$mod SHIFT,W,exec,looking-glass-client -f /dev/shm/looking-glass2"
            "$mod,Q,exec,${config.programs.swaylock.package}"
        ];
    };
}
