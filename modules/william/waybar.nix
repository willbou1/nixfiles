{ pkgs, ... }:

{
    programs.waybar = {
        enable = true;
        settings = {
            waybar = {
                margin-top = 5;
                margin-left = 10;
                margin-right = 10;
                layer = "top";
                position = "top";
                output = [
                    "eDP-1"
                ];
                modules-left = [ "hyprland/workspaces" ];
                modules-center = [ "hyprland/window" ];
                modules-right = [ "pulseaudio" "backlight" "memory" "temperature" "battery" ];
            };
        };
        style = ''
            * {
                border-radius: 20px;
            }
            window#waybar {
                background: transparent;
            }
        '';
    };
}
