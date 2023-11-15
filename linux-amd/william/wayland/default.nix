{ pkgs, ... }:

{
    imports = [
        ./hyprland.nix
        ./eww
    ];

    home.packages = with pkgs; [
        eww-wayland
        sysstat
        jq
        lm_sensors
    ];
}
