{ pkgs, ... }:

{
    imports = [
        ./theme.nix
        ./ime.nix
        ./xorg
        ./wayland
    ];

    services.dunst.settings.global.offset = "20x100";
}
