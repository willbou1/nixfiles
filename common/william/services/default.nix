{ pkgs, config, ... }: let
in {
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
        easyeffects.enable = true;
    };
}
