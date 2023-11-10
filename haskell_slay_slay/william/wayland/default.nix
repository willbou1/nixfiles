{
    imports = [
        ./pm.nix
        ./kanshi.nix
        ./pm.nix
        ./waybar.nix
        ./hyprland.nix
    ];
    services.dunst.settings.global.offset = "20x22";
}
