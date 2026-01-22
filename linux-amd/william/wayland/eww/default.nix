{
  lib,
  pkgs,
  config,
  ...
}:
with builtins;
with lib;
with config.lib.stylix.colors;
let
  vars = with config.home; {
    "@border@" = toString borderSize;
    "@out_gap@" = toString (gapSize * 1.5);
    "@in_gap@" = toString gapSize;
    "@bar_y_secondary@" =
      if verticalDisplays
      then toString (gapSize / 2)
      else toString (1440 - gapSize / 2 - ewwHeight);
    "@bar_y_main@" = toString (1440 - gapSize / 2 - ewwHeight);
    "@bar_height@" = toString ewwHeight;
    "@width@" = "3440";
    "@height@" = "1440";
    "@bar_right_width@" = "500";
    "@bar_wm_width@" = "460";
    "@bar_music_width@" = "320";
    "@bar_controls_width@" = "590";
    "@bar_net_width@" = "380";
    "@bar_cpu_width@" = "810";
    "@bar_mem_width@" = "380";
    "@bar_temp_width@" = "380";
  };
  varNames = attrNames vars;
  varVals = attrValues vars;
  scssColors =
    (mine.generateBase16ColorFile
      withHashtag
      (i: c: "$color${toString i}: ${c};")
    ) + "\n" + ''
      $background: ${withHashtag.base00};
      $foreground: ${withHashtag.base05};
    '';
  yuckColors =
    (mine.generateBase16ColorFile
      withHashtag
      (i: c: ''(defvar color${toString i} "${c}")'')
    ) + "\n" + ''
      (defvar background "${withHashtag.base00}")
      (defvar foreground "${withHashtag.base05}")
    '';
in {
  options.home.ewwHeight = mkOption {
    type = types.int;
  };
  config = {
    home.ewwHeight = 65;
    # TODO find better way to provide packages
    home.packages = with pkgs; [
      eww
      jq
      ddcutil
      python3
      sysstat
      lm_sensors
    ];

    xdg.configFile =
      (listToAttrs (map (n: {
        name = "eww/scripts/${n}";
        value =
          if hasSuffix "sh" n
          then {
            source =
              pkgs.writeShellScript n
              (readFile (./scripts + "/${n}"));
          }
          else if hasSuffix "py" n
          then {
            text =
              ''
                #! ${pkgs.python3}/bin/python
              ''
              + readFile (./scripts + "/${n}");
          }
          else {source = ./scripts + "/${n}";};
      }) (attrNames (readDir ./scripts))))
      // (listToAttrs (map (n: {
        name = "eww/defs/${n}";
        value.text =
          replaceStrings varNames varVals
          (readFile (./defs + "/${n}"));
      }) (attrNames (readDir ./defs))))
      // {
        "eww/eww.yuck".text =
          yuckColors
          + (replaceStrings varNames varVals (readFile ./eww.yuck));
        "eww/eww.scss".text =
          scssColors
          + (replaceStrings varNames varVals (readFile ./eww.scss));
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
