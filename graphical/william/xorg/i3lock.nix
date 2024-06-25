{ lib, config, pkgs, ... }:
with config.lib.stylix.colors; let
opacity = builtins.ceil (config.stylix.opacity.desktop * 100);
hexOpacity =  lib.toHexString (opacity * 255 / 100);
rgba = color: color + hexOpacity;
inside = rgba base01-hex;
outside = rgba base01-hex;
ring = rgba base05-hex;
text = base05-hex;
positive = rgba base0B-hex;
negative = rgba base08-hex;
i3lockCustom = pkgs.writeShellScriptBin "i3lock-custom" ''
    ${pkgs.i3lock-color}/bin/i3lock -k \
       --inside-color=${inside} --ring-color=${ring} \
       --insidever-color=${inside} --ringver-color=${positive} \
       --insidewrong-color=${inside} --ringwrong-color=${negative} \
       --keyhl-color=${positive} --bshl-color=${negative} \
       --line-color=${config.lib.stylix.colors.base00}ff --separator-color=00000000 \
       --time-color=${text} --date-color=${text} --greeter-color=${text} \
       --verif-color=${text} --wrong-color=${text} --modif-color=${text} \
       --radius 130 --ring-width 10 -B 7
'';
in {
    services.screen-locker = {
        enable = true;
        lockCmd = "${i3lockCustom}/bin/i3lock-custom";
        xss-lock.screensaverCycle = 1000;
    };
    home.packages = [ i3lockCustom ];
}
