{ pkgs, ... }:

{
    imports = [
        ./theme.nix
        ./ime.nix
        ./wayland
        ./mpv.nix
        ./xorg.nix
    ];

    services.dunst.settings.global.offset = "20x100";
    home = {
        gapSize = 20;
        borderSize = 5;
    };
}
