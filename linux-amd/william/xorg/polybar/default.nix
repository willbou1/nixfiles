{ pkgs, config, lib, ... }: let
opacity = builtins.ceil (config.stylix.opacity.desktop * 100);
hexOpacity =  lib.toHexString (opacity * 255 / 100);
rgba = color: "#${hexOpacity}${color}";
backlight = pkgs.writeShellScript "backlight.sh"
    (builtins.readFile ./backlight.sh);
in with config.lib.stylix.colors; {
    systemd.user.services.polybar.Service.Environment = with pkgs; (lib.mkForce ''
        PATH=${polybar}/bin:${xorg.xrandr}/bin:${gnugrep}/bin:${coreutils-full}/bin:${bc}/bin:${systemd}/bin:${gawk}/bin:${imagemagick}/bin:${pkgs.xclip}/bin:/run/wrappers/bin
    '');
    services.polybar = {
        enable = true;
        script = ''
            #! /usr/bin/env/bash
            for monitor in $(xrandr --query | grep " connected" | cut -d" " -f1); do
                MONITOR=$monitor polybar --reload example &
            done
        '';
        settings = rec {
            settings = {
                screenchange-reload = "true";
                pseudo-transparency = "false";
            };
            colors = {
                background = rgba base00;
                foreground = withHashtag.base05;
                foreground-alt = withHashtag.base04;
                primary = withHashtag.base09;
                secondary = withHashtag.base08;
                alert = withHashtag.base0A;
            };
            "bar/example" = {
                monitor = "\${env:MONITOR}";
                width = "88%";
                offset-x = "6%";
                height = "2.5%";
                radius = 15;
                background = "\${colors.background}";
                foreground = "\${colors.foreground}";
                line-size = 3;
                border-size = 10;
                border-color = "#00000000";
                padding-left = 2;
                padding-right = 2;
                module-margin = 1;
                separator = "";
                separator-foreground = "\${colors.disabled}";
                font-0 = "monospace;2";
                font-1 = "JoyPixels:scale=10;";
                font-2 = "RobotoMono Nerd Font:pixelsize=10;2";
                modules-left = "uname xworkspaces layout xwindow";
                modules-right = "filesystem pulseaudio bluetooth memory cpu vpn wlan backlight date-seoul date";
                cursor-click = "pointer";
                cursor-scroll = "ns-resize";
                enable-ipc = true;
                tray-position = "right";
                tray-scale = 1.0;
                tray-maxsize = 16;
            };
            "module/xworkspaces" = {
                type = "internal/xworkspaces";
                label-active = "%name%";
                label-active-background = "\${colors.background-alt}";
                label-active-underline = "\${colors.primary}";
                label-active-padding = 1;

                label-occupied = "%name%";
                label-occupied-padding = 1;

                label-urgent = "%name%";
                label-urgent-background = "\${colors.alert}";
                label-urgent-padding = 1;

                label-empty = "%name%";
                label-empty-foreground = "\${colors.disabled}";
                label-empty-padding = 1;
            };
            "module/xwindow" = {
                type = "internal/xwindow";
                label = "%title:0:60:...%";
            };
            "module/filesystem" = {
                type = "internal/fs";
                interval = 25;
                mount-0 = "/";
                mount-1 = "/home/william";
                label-mounted = "%{F${colors.primary}}%mountpoint%%{F-} %percentage_used%%";
                label-unmounted = "%mountpoint% not mounted";
                label-unmounted-foreground = "\${colors.disabled}";
            };
            "module/pulseaudio" = {
                type = "internal/pulseaudio";
                format-volume-prefix = "VOL ";
                format-volume-prefix-foreground = "\${colors.primary}";
                format-volume = "<label-volume>";
                label-volume = "%percentage%%";
                label-muted = "muted";
                label-muted-foreground = "\${colors.disabled}";
            };
            "module/xkeyboard" = {
                type = "internal/xkeyboard";
                blacklist-0 = "num lock";
                label-layout = "%layout%";
                label-layout-foreground = "\${colors.primary}";
                label-indicator-padding = 2;
                label-indicator-margin = 1;
                label-indicator-foreground = "\${colors.background}";
                label-indicator-background = "\${colors.secondary}";
            };
            "module/memory" = {
                type = "internal/memory";
                interval = 2;
                format-prefix = "RAM ";
                format-prefix-foreground = "\${colors.primary}";
                label = "%percentage_used:2%%";
            };
            "module/cpu" = {
                type = "internal/cpu";
                interval = 2;
                format-prefix = "CPU ";
                format-prefix-foreground = "\${colors.primary}";
                label = "%percentage:2%%";
            };
            "network-base" = {
                type = "internal/network";
                interval = 5;
                format-connected = "<label-connected>";
                format-disconnected = "<label-disconnected>";
                label-disconnected = "%{F${colors.primary}}%ifname%%{F${colors.alert}} disconnected";
            };
            "module/wlan" = {
                "inherit" = "network-base";
                interface-type = "wireless";
                label-connected = "%{F${colors.primary}}%ifname%%{F${colors.secondary}} %essid% %{F-}%local_ip%";
            };
            "module/eth" = {
                "inherit" = "network-base";
                interface-type = "wired";
                label-connected = "%{F${colors.primary}}%ifname%%{F-} %local_ip%";
            };
            "module/date" = {
                type = "internal/date";
                interval = 1;
                date = "%I:%M %p";
                date-alt = "%Y-%m-%d %H:%M:%S";
                label = "%date%";
                label-foreground = "\${colors.disabled}";
                format-prefix = "CA ";
                format-prefix-foreground = "\${colors.primary}";
            };
            "module/date-seoul" = {
                type = "custom/script";
                format-prefix = "SE ";
                format-prefix-foreground = "\${colors.primary}";
                exec = ''TZ=Asia/Seoul date +"%I:%M %p"'';
                interval = 30;
            };
            "module/bluetooth" = {
                type = "custom/script";
                exec = "${pkgs.rofi-bluetooth}/bin/rofi-bluetooth --status";
                interval = 1;
                click-left = "${pkgs.rofi-bluetooth}/bin/rofi-bluetooth &";
            };
            "module/backlight" = {
                type = "custom/script";
                interval = 1;
                format-prefix = "☀️ ";
                exec = "${backlight}";
                click-left = "BUTTON=1 ${backlight}";
                click-middle = "BUTTON=2 ${backlight}";
                click-right = "BUTTON=3 ${backlight}";
                scroll-up = "BUTTON=4 ${backlight}";
                scroll-down = "BUTTON=5 ${backlight}";
            };
            "module/vpn" = {
                type = "custom/script";
                format-prefix = "🔒 ";
                interval = 1;
                exec = "systemctl list-units --type=service --state=running | awk '/openvpn/ { rc = 1; split($1, DASH, \"-\"); split(DASH[2], LOC, \".\"); print LOC[1]; next } END { if (rc != 1) { print \"Disconnected\"} }'";
                #click-left = "~/.config/polybar/scripts/vpm_switch.sh";
                #click-right = "expressvpn disconnect";
            };
            "module/uname" = {
                type = "custom/script";
                interval = 0;
                format-prefix = "🐧 ";
                exec = "uname -r | awk '/.*git.*/ {print \"git\"} /.*lts.*/ {print \"lts\"} /.*rt.*/ {print \"rt\"} /.*zen.*/ {print \"zen\"}'";
                click-left = "alacritty";
                click-middle = "firefox-beta";
                click-right = "light-locker-command -l";
            };
        };
    };
}
