{
    imports = [
        ./mpv.nix
        ./theme.nix
        ./wayland
        ./xorg.nix
    ];
    home.persistence."/persist/home/william".directories = [
        "priv"
    ];
    home = {
        gapSize = 10;
        borderSize = 3;
    };
}
