{
    imports = [
        ./mpv.nix
        ./theme.nix
        ./wayland
        ./xorg.nix
        ./cli.nix
    ];
    home.persistence."/persist/home/william".directories = [
        "priv"
    ];
    home = {
        gapSize = 8;
        borderSize = 3;
    };
    home.monitors = [
        {
            wlrName = "eDP-1";
            xrandrName = "eDP-1";
            width = 3840;
            height = 2400;
            rate = 60;
            x = 0;
            y = 0;
            hScale = 2;
            vScale = 2;
        }
    ];
}
