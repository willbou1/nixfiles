{ pkgs, config, ... }:
let
    systemctl = config.systemd.user.systemctlPath;
    brillo = "${pkgs.brillo}/bin/brillo";
    brilloDim = pkgs.writeShellScript "brilloDim" ''
        if ! ${pkgs.procps}/bin/pgrep '.*swaylock.*'; then
            echo 0 > /sys/devices/platform/dell-laptop/leds/dell::kbd_backlight/brightness
            ${brillo} -O
            ${brillo} -u 150000 -S 0
        fi
    '';
    brilloRestore = pkgs.writeShellScript "brilloRestore" ''
        if ! ${pkgs.procps}/bin/pgrep '.*swaylock.*'; then
            echo 1 > /sys/devices/platform/dell-laptop/leds/dell::kbd_backlight/brightness
            ${brillo} -u 150000 -I
        fi
    '';
    lock = pkgs.writeShellScript "lock" ''
        if ! ${pkgs.procps}/bin/pgrep '.*swaylock.*'; then
            {
                ${config.programs.swaylock.package}/bin/swaylock "$@"
                ${brilloRestore}
            } &
            disown -a
        fi
    '';
in {
    wayland.windowManager.hyprland.settings.bind = [
        "$mod,Q,exec,${brilloDim}; ${lock}"
    ];
    services.swayidle = {
        timeouts = [
            {
                timeout = 300;
                command = "${brilloDim}";
                resumeCommand = "${brilloRestore}";
            }
            {
                timeout = 600;
                command = "${lock}";
            }
            {
                timeout = 700;
                command = "${pkgs.hyprland}/bin/hyprctl dispatch dpms off";
                resumeCommand = "${pkgs.hyprland}/bin/hyprctl dispatch dpms on";
            }
            {
                timeout = 800;
                command = "${systemctl} suspend";
            }
        ];
        events = [
            {
                event = "before-sleep";
                command = "${lock} --grace 0";
            }
            {
                event = "after-resume";
                command = "sleep 1; ${pkgs.hyprland}/bin/hyprctl dispatch dpms on";
            }
        ];
    };
}
