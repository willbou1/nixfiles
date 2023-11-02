{ pkgs, ... }:

{
    imports = [
        ./hyprland.nix
        ./theme.nix
    ];

    home.packages = with pkgs; [
        eww-wayland
    ];
    xdg.configFile."eww".source = ./eww;
}
