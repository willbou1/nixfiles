{ config, pkgs, lib, ...}:

{
    programs.qutebrowser = {
        keyBindings.normal = {
            ",m" = "spawn mpv {url}";
            ",M" = "hint --rapid links spawn mpv {hint-url}";
        };
    };
}
