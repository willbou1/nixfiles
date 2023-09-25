{ config, pkgs, inputs, ... }: let
swaylock = "${config.programs.swaylock.package}/bin/swaylock";
brillo = "${pkgs.brillo}/bin/brillo";
in {
    services.swayidle = {
        enable = true;
        systemdTarget = "hyprland-session.target";
        timeouts = [
            {
                timeout = 300;
                command = "${brillo} -O; ${brillo} -u 150000 -S 15";
                resumeCommand = "${brillo} -u 150000 -I";
            }
            {
                timeout = 600;
                command = "${swaylock} -F";
            }
            {
                timeout = 700;
                command = "${pkgs.hyprland}/bin/hyprctl dispatch dpms off";
                resumeCommand = "${pkgs.hyprland}/bin/hyprctl dispatch dpms on";
            }
            {
                timeout = 800;
                command = "systemctl suspend";
            }
        ];
        events = [
            {
                event = "before-sleep";
                command = "${pkgs.playerctl}/bin/playerctl pause";
            }
            {
                event = "before-sleep";
                command = "${swaylock} -F";
            }
        ];
    };

    programs.swaylock = {
        enable = true;
        package = pkgs.swaylock-effects;
        settings = {
            daemonize = true;
            screenshots = true;
            clock = true;
            indicator = true;
            grace = 5;
            fade-in =1;
            effect-blur = "7x5";
            effect-vignette = "0.45:0";
            indicator-thickness = 7;
            indicator-radius = 130;
        };
    };

}
