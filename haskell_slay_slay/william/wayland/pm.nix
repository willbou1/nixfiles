{ pkgs, ... }: let
brillo = "${pkgs.brillo}/bin/brillo";
in {
    services.swayidle = {
        enable = true;
        systemdTarget = "hyprland-session.target";
        timeouts = [
            {
                timeout = 300;
                command = "${brillo} -O; ${brillo} -u 150000 -S 0";
                resumeCommand = "${brillo} -u 150000 -I";
            }
        ];
    };
}
