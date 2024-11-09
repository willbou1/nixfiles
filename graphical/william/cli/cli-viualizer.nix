{ pkgs, ... }:

{
    xdg.configFile = {
        "vis/config".text = ''
            colors.override.terminal=false
            colors.scheme=kitty
            visualizer.spectrum.bar.width=1
        '';
        "vis/colors/kitty".text = ''
            gradient=false
            2
            3
            1
        '';
    };

    home.packages = with pkgs; [
        cli-visualizer
    ];
}
