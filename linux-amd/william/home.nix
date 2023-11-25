{ pkgs, lib, ... }:
with lib;
{
    imports = [
        ./theme.nix
        ./ime.nix
        ./wayland
        ./mpv.nix
        ./xorg.nix
    ];

    home = {
        gapSize = 20;
        borderSize = 5;
    };
    programs.qutebrowser.settings.zoom.default = mkForce "175%";
}
