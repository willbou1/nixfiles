{ pkgs, ... }:

{
    home.packages = with pkgs; [
        eww-wayland
        jq
        ddcutil
    ];
    xdg.configFile."eww".source = ./eww;
}
