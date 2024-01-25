{ pkgs, lib, ... }:
with lib;
{
    imports = [
        ./theme.nix
        ./wayland
        ./mpv.nix
        ./xorg.nix
    ];

    home = {
        gapSize = 20;
        borderSize = 5;
    };

    # little adjustments
    programs.qutebrowser.settings.zoom.default = mkForce "175%";
    services.dunst.settings.global = {
        monitor = mkForce 1;
        origin = mkForce "top-right";
    };
}
