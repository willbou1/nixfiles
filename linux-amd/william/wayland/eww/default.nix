{ pkgs, ... }:

{
    home.packages = with pkgs; [
        eww-wayland
        sysstat
        jq
        lm_sensors
        ddcutil
    ];
    xdg.configFile."eww".source = ./eww;
}
