{ config, ... }: let
opacity = builtins.toString config.stylix.opacity.desktop;
in {
    programs.waybar = {
        enable = true;
        settings = {
            waybar = {
                spacing = 8;
                margin-top = 6;
                margin-left = 10;
                margin-right = 10;
                layer = "top";
                position = "top";
                output = [
                    "eDP-1"
                ];
                modules-left = [ "custom/nix" "hyprland/workspaces" "hyprland/window" ];
                modules-center = [ "clock" ];
                modules-right = [ "custom/kime" "network" "pulseaudio" "backlight" "memory" "cpu" "temperature" "battery" ];
                cpu.format = "  {usage}%";
                memory.format = "  {}%";
                clock.format = "{:%I:%M:%p}";
                "custom/nix".format = "󱄅";
                "custom/kime" = {
                    format = "󰌌  {}";
                    exec = ''
echo ca
kime-indicator 2>&1 |
while read -r line; do
    case "$line" in
        *"Hangul"*)
            echo ko
            ;;
        *"Latin"*)
            echo ca
            ;;
    esac
done
                    '';
                };
                "hyprland/window" = {
                    separate-outputs = true;
                };
                "hyprland/workspaces" = {
                    persistent_workspaces = {
                        "*" = 9;
                    };
                    format = "{icon}";
                    format-icons = {
                        "1" = "";
                        "2" = "";
                        "3" = "";
                        "4" = "󰿎";
                        "5" = "󰭹";
                        "6" = "";
                        "7" = "";
                        "8" = "";
                        "9" = "";
                    };
                };
                battery = {
                    states = {
                        warning = 30;
                        critical = 15;
                    };
                    format = "{icon}   {capacity}%";
                    format-charging = "󰂅  {capacity}%";
                    format-plugged = "  {capacity}%";
                    format-icons = ["" "" "" "" ""];
                };
                network = {
                    format-wifi = "  {essid} ({signalStrength}%)";
                    format-ethernet = "󰈁  {ifname}";
                    tooltip-format = "󰈁  {ifname} via {gwaddr}";
                    format-linked = "  {ifname} (No IP)";
                    format-disconnected = "Disconnected ⚠ {ifname}";
                    format-alt = "  {ifname}: {ipaddr}/{cidr}";
                };
                pulseaudio = {
                    format = "{icon}  {volume}%   {format_source}";
                    format-bluetooth = " {icon}  {volume}% {format_source}";
                    format-bluetooth-muted = "󰝟  {icon}  {format_source}";
                    format-muted = "󰝟  {format_source}";
                    format-source = " {volume}%";
                    format-source-muted = "";
                    format-icons.default = ["" "" ""];
                };
                temperature.format = "󰔏  {temperature}°C";
                backlight = {
                    format = "{icon}  {percent}%";
                    format-icons = [ "󰃜" "󰃛" "󰃚" ];
                };
            };
        };
        style = ''
            * {
                border-radius: 20px;
            }
            window#waybar {
                background: transparent;
            }
            #custom-nix {
                color: @base09;
                margin-right: 10px;
                font-size: 25pt;
            }
            .modules-left, .modules-center, .modules-right {
                background-color: alpha(@base00, ${opacity});
                border: solid alpha(@base03, ${opacity}) 3px;
                padding: 0 10px 0 10px;
            }
            .modules-center {
                margin-left: 10px;
                margin-right: 10px;
            }
            .modules-left #workspaces button {
                border: none;
                color: @base06;
            }
            .modules-left #workspaces button.empty {
                border: none;
                color: @base05;
            }
            .modules-left #workspaces button.active, .modules-left #workspaces button.focused {
                border: none;
                color: @base04;
            }
        '';
    };
}
