{ pkgs, ... }:

{
    imports = [
        ./hyprland.nix
    ];

    home.packages = with pkgs; [
        eww-wayland
        sysstat
        jq
        lm_sensors
    ];
    xdg.configFile."eww".source = ./eww;
}
