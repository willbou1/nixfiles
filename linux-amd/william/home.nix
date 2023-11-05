{ pkgs, ... }:

{
    imports = [
        ./hyprland.nix
        ./theme.nix
        ./ime.nix
        ./herbstluftwm.nix
        ./picom.nix
        ./i3lock.nix
    ];

    services.dunst.settings.global.offset = "20x100";

    home.packages = with pkgs; [
        eww-wayland
        sysstat
        jq
        lm_sensors
    ];
    xdg.configFile."eww".source = ./eww;
}
