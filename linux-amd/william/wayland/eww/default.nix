{ lib, pkgs, config, ... }:
with builtins;
with lib;
with config.lib.stylix.colors.withHashtag;
let
    vars = {
        "@border@" = toString config.home.borderSize;
        "@out_gap@" = toString (config.home.gapSize * 1.5);
        "@in_gap@" = toString config.home.gapSize;
        "@bar_y@" = toString (config.home.gapSize / 2);
        "@bar_height@" = toString config.home.ewwHeight;
        "@width@" = "3440";
        "@height@" = "1440";
        "@bar_right_width@" = "500";
        "@bar_wm_width@" = "460";
        "@bar_music_width@" = "320";
        "@bar_controls_width@" = "560";
        "@bar_net_width@" = "390";
        "@bar_cpu_width@" = "810";
        "@bar_mem_width@" = "380";
        "@bar_temp_width@" = "380";
    };
    varNames = attrNames vars;
    varVals = attrValues vars;
    scssColors = ''
        $background: ${base00};
        $foreground: ${base05};
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
        $color10: ${base0A};
        $color11: ${base0B};
        $color12: ${base0C};
        $color13: ${base0D};
        $color14: ${base0E};
        $color15: ${base0F};
    '';
    yuckColors = ''
        (defvar background "${base00}")
        (defvar foreground "${base05}")
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
        (defvar color10 "${base0A}")
        (defvar color11 "${base0B}")
        (defvar color12 "${base0C}")
        (defvar color13 "${base0D}")
        (defvar color14 "${base0E}")
        (defvar color15 "${base0F}")
    '';
in {
    options.home.ewwHeight = mkOption {
        type = types.int;
    };
    config = {
        home.ewwHeight = 65;
        home.packages = with pkgs; [
            eww-wayland
            jq
            ddcutil
        ];
        
        xdg.configFile = (listToAttrs (map (n: {
            name = "eww/scripts/${n}";
            value = if hasSuffix "sh" n
                then { source = pkgs.writeShellScript n
                    (readFile (./scripts + "/${n}")); }
                else if hasSuffix "py" n
                then { text = ''
                    #! ${pkgs.python3}/bin/python
                '' + readFile (./scripts + "/${n}"); }
                else { source = ./scripts + "/${n}"; };
        }) (attrNames (readDir ./scripts)))) // 
        (listToAttrs (map (n: {
            name = "eww/defs/${n}";
            value.text = replaceStrings varNames varVals
                (readFile (./defs + "/${n}"));
        }) (attrNames (readDir ./defs)))) // {
            "eww/eww.yuck".text = yuckColors +
                (replaceStrings varNames varVals (readFile ./eww.yuck));
            "eww/eww.scss".text = scssColors +
                (replaceStrings varNames varVals (readFile ./eww.scss));
            "eww/styles".source = ./styles;
            "eww/images".source = ./images;
            "eww/launch.sh".source = pkgs.writeShellScript "launch.sh" ''
                pkill ".*eww.*"
                eww daemon
                eww open bar_wm_mon1
                eww open bar_wm_mon2
                eww open bar_music
                eww open bar_cpu
                eww open bar_mem
                eww open bar_controls
                eww open bar_right_mon1
                eww open bar_right_mon2
                eww open bar_net
                eww open bar_temp
            '';
        };
    };
}
