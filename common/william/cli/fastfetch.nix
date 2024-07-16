{ pkgs, lib, ... }:
with lib;
let
    fastfetch = "${pkgs.fastfetch}/bin/fastfetch";
in {

    programs.fish.functions.fish_greeting = ''
        tty | grep tty > /dev/null && ${fastfetch} || ${fastfetch} -l (random choice ~/.config/fastfetch/images/*)
    '';

    programs.fastfetch = {
        enable = true;
        settings = {
            modules = [
                "os"
                "battery"
                "kernel"
                "cpu"
                "gpu"
                "memory"
                "swap"
                {
                    type = "disk";
                    key = "Disk";
                    folders = "/:/home/william:/persist";
                }
            ];
        };
    };

    xdg.configFile = {
        "fastfetch/images".source = ../../../resources/splash/images;
    };
}
