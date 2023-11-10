{ pkgs, ... }:

{
    imports = [
        ./theme.nix
        ./ime.nix
        ./xorg
        ./wayland
        ./mpv.nix
    ];

    services.dunst.settings.global.offset = "20x100";
}
