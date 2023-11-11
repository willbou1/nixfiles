{ pkgs, lib, config, ... }:
with builtins;
let
    monitors = concatStringsSep " " (map (m: "--output ${m.name} --mode ${toString m.width}x${toString m.height} --rate ${toString m.rate} --scale ${toString (1.0 / m.hScale)}x${toString (1.0 / m.vScale)} --pos ${toString m.x}x${toString m.y}") config.home.monitors);
in {
    imports = [
        ./picom.nix
        ./i3lock.nix
        ./herbstluftwm.nix
        ./glava
        ./polybar
        ./rofi
        ./dwm
    ];

    home.file.".xinitrc".source = pkgs.writeShellScript ".xinitrc" ''
        ${pkgs.dbus}/bin/dbus-update-activation-environment --systemd DISPLAY WAYLAND_DISPLAY HYPRLAND_INSTANCE_SIGNATURE XDG_CURRENT_DESKTOP XAUTHORITY XDG_SESSION_ID
        xrandr ${monitors}
        ${pkgs.kime}/bin/kime -D &
        xset s on
        xset s 1200
        xset s blank
        if [[ $1 == "herbstluftwm" ]]; then
            exec herbstluftwm
        elif [[ $1 == "dwm" ]]; then
            exec dwm
        else
            echo "Choose a window manager"
        fi
    '';
}
