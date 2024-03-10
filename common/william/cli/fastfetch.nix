{ pkgs, lib, ... }:
with lib;
let
    fastfetch = "${pkgs.fastfetch}/bin/fastfetch";
in {
    home.packages = with pkgs; [
        pkgs.fastfetch
    ];

    programs.fish.functions.fish_greeting = ''
        tty | grep tty > /dev/null && ${fastfetch} || ${fastfetch} -l (random choice ~/.config/fastfetch/images/*)
    '';

    xdg.configFile = {
        "fastfetch/config.conf".source = ./fastfetch.conf;
        "fastfetch/images".source = ../../../resources/splash/images;
    };
}
