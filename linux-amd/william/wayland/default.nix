{ pkgs, lib, ... }:
with lib;
{
    imports = mine.autoInclude ./. [];

    home.packages = with pkgs; [
        eww-wayland
        sysstat
        jq
        lm_sensors
    ];
}
