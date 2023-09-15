{
    imports = [
        ./pm.nix
        ./dunst.nix
    ];
    services = {
        udiskie = {
            enable = true;
            tray = "never";
        };
        ssh-agent.enable = true;
        kanshi = {
            enable = true;
            systemdTarget = "hyprland-session.target";
            profiles = {
                undocked.outputs = [{
                    criteria = "eDP-1";
                    mode = "3840x2400@60Hz";
                    position = "0,0";
                    scale = 2.0;
                }];
            };
        };
    };
}
