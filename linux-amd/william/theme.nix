{ config, pkgs, inputs, lib, ... }:

with config.lib.stylix.colors.withHashtag; {
    xdg.configFile."stylix/colors.yuck".text = ''
        (defvar background "${base00}")
        (defvar foreground "${base07}")
        (defvar color0 "${base00}")
        (defvar color1 "${base01}")
        (defvar color2 "${base02}")
        (defvar color3 "${base03}")
        (defvar color4 "${base04}")
        (defvar color5 "${base05}")
        (defvar color6 "${base06}")
        (defvar color7 "${base07}")
        (defvar color8 "${base08}")
        (defvar color9 "${base09}")
        (defvar color10 "${base10}")
        (defvar color11 "${base11}")
        (defvar color12 "${base12}")
        (defvar color13 "${base13}")
        (defvar color14 "${base14}")
        (defvar color15 "${base15}")
    '';
    xdg.configFile."stylix/colors.scss".text = ''
        $background: ${base00};
        $foreground: ${base07};

        // Colors
        $color0: ${base00};
        $color1: ${base01};
        $color2: ${base02};
        $color3: ${base03};
        $color4: ${base04};
        $color5: ${base05};
        $color6: ${base06};
        $color7: ${base07};
        $color8: ${base08};
        $color9: ${base09};
        $color10: ${base10};
        $color11: ${base11};
        $color12: ${base12};
        $color13: ${base13};
        $color14: ${base14};
        $color15: ${base15};
    '';
}
