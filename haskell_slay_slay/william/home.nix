{
    imports = [
        ./services
        ./hyprland.nix
        ./waybar.nix
        ./mpv.nix
        ./theme.nix
    ];
    home.persistence."/persist/home/william".directories = [
        "priv"
    ];
}
