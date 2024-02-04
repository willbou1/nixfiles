{ pkgs, lib, ... }:
with lib;
{
    imports = mine.autoInclude ./. [
        ./william
    ];

    environment.shellAliases = {
        "nr" = "sudo nixos-rebuild --show-trace --flake /etc/nixos#linux-amd switch";
    };
    programs.i3lock = {
      enable = true;  
      package = pkgs.i3lock-color;
    };

    services.udev.extraRules = ''
        KERNEL=="i2c-[0-9]*", GROUP="i2c", MODE="0660"
    '';
    users = {
        groups.i2c = {};
        users.william.extraGroups = [ "i2c" ];
    };
}
