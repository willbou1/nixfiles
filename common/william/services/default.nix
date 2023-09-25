{ pkgs, config, ... }: let
in {
    imports = [
        ./pm.nix
        ./dunst.nix
        ./kanshi.nix
    ];
    services = {
        udiskie = {
            enable = true;
            tray = "never";
        };
        ssh-agent.enable = true;
    };
}
