{
    imports = [
        ./services
        ./hyprland.nix
        ./waybar.nix
    ];
    home.persistence."/persist/home/william".directories = [
        "priv"
    ];
}
