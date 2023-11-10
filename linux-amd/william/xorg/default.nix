{ pkgs, ... }:

{
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
        xrandr --output DisplayPort-0 --mode 3440x1440 --rate 99.98 --set TearFree on --output HDMI-A-0 --mode 3440x1440 --rate 99.98 --set TearFree on --left-of DisplayPort-0
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
