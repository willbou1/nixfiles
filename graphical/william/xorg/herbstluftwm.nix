{
  lib,
  config,
  pkgs,
  ...
}:
with builtins; let
  cfg = config.xsession.windowManager.herbstluftwm;
  scaledWidth = m: ceil (m.width * (1.0 / m.hScale));
  scaledHeight = m: ceil (m.height * (1.0 / m.vScale));
  monitors = concatStringsSep " " (map (m: "${toString (scaledWidth m)}x${toString (scaledHeight m)}+${toString m.x}+${toString m.y}") config.home.monitors);
  mod = "Mod4";
  resizeStep = "0.02";
  opacity = ceil (config.stylix.opacity.desktop * 100);
  hexOpacity = lib.toHexString (opacity * 255 / 100);
  rgba = color: color + hexOpacity;
  hc = "herbstclient";
  flattenInternal = lineStart: position: attrs:
    concatStringsSep "\n" (
      attrValues (mapAttrs (
          n: v:
            if typeOf v == "set"
            then flattenInternal lineStart "${position}${n}." v
            else "${lineStart}${position}${n} '${toString v}'"
        )
        attrs)
    );
  flatten = lineStart: attrs: flattenInternal lineStart "" attrs;
in
  with config.lib.stylix.colors.withHashtag; {
    options.xsession.windowManager.herbstluftwm.attributes = lib.mkOption {
      type = lib.types.attrs;
      default = {};
    };
    config = {
      xsession.windowManager.herbstluftwm = rec {
        enable = true;
        settings = {
          frame_border_active_color = base0A;
          frame_border_normal_color = base03;
          frame_bg_normal_color = base03;
          frame_bg_active_color = base0A;
          frame_active_opacity = opacity;
          frame_normal_opacity = opacity;
          frame_border_width = config.home.borderSize / 2;
          always_show_frame = 0;
          frame_bg_transparent = 1;
          frame_transparent_width = 1;
          window_gap = config.home.gapSize;
          window_border_inner_width = 0;
          frame_padding = 0;
          frame_gap = config.home.gapSize / 2;
          smart_window_surroundings = 0;
          smart_frame_surroundings = 1;
          mouse_recenter_gap = 0;
          update_dragged_clients = 1;
          hide_covered_windows = 1;
          tree_style = "╾│ ├└╼─┐";
        };
        attributes.theme = {
          floating = {
            border_width = config.home.borderSize;
            outer_width = 1;
            outer_color = "black";
          };
          active = {
            color = rgba base0A;
            inner_color = rgba base0A;
            outer_color = rgba base0A;
          };
          normal.color = rgba base03;
          urgent.color = rgba base07;
          inner_width = 1;
          inner_color = "black";
          border_width = config.home.borderSize;
          background_color = base00;
        };
        rules = [
          "focus=on"
          "floatplacement=center"
          "windowtype~'_NET_WM_WINDOW_TYPE_(DIALOG|UTILITY|SPLASH)' floating=on"
          "windowtype='_NET_WM_WINDOW_TYPE_DIALOG' focus=on"
          "windowtype~'_NET_WM_WINDOW_TYPE_(NOTIFICATION|DOCK|DESKTOP)' manage=off"
          "class~'Element' tag=5"
          "class~'mpv' tag=4"
          "class~'SVPManager' tag=9"
        ];
        tags = builtins.genList (x: builtins.toString (x + 1)) 9;
        mousebinds = {
          "${mod}-Button1" = "move";
          "${mod}-Button2" = "zoom";
          "${mod}-Button3" = "resize";
        };
        keybinds =
          {
            "XF86AudioRaiseVolume" = "spawn ${pkgs.pulseaudio}/bin/pactl set-sink-volume @DEFAULT_SINK@ +5%";
            "XF86AudioLowerVolume" = "spawn ${pkgs.pulseaudio}/bin/pactl set-sink-volume @DEFAULT_SINK@ -5%";
            "XF86AudioMute" = "spawn ${pkgs.pulseaudio}/bin/pactl set-sink-mute @DEFAULT_SINK@ toggle";
            "XF86AudioPlay" = "spawn ${pkgs.playerctl}/bin/playerctl play-pause";
            "XF86AudioPause" = "spawn ${pkgs.playerctl}/bin/playerctl pause";
            "XF86AudioStop" = "spawn ${pkgs.playerctl}/bin/playerctl pause";
            "XF86AudioPrev" = "spawn ${pkgs.playerctl}/bin/playerctl previous";
            "XF86AudioNext" = "spawn ${pkgs.playerctl}/bin/playerctl next";

            "${mod}-Shift-q" = "quit";
            "${mod}-q" = "spawn i3lock-custom";
            "${mod}-Shift-r" = "reload";
            "${mod}-Shift-c" = "close";
            "${mod}-Return" = "spawn ${config.home.terminal}";

            "${mod}-b" = "spawn qutebrowser";
            "${mod}-Shift-b" = "spawn qutebrowser ':open -p'";
            "${mod}-d" = "spawn rofi -show combi";
            "${mod}-e" = "spawn emacsclient -c";

            "${mod}-h" = "focus left";
            "${mod}-j" = "focus down";
            "${mod}-k" = "focus up";
            "${mod}-l" = "focus right";

            "${mod}-Shift-h" = "shift left";
            "${mod}-Shift-j" = "shift down";
            "${mod}-Shift-k" = "shift up";
            "${mod}-Shift-l" = "shift right";

            "${mod}-Control-Shift-h" = "mirror horizontal";
            "${mod}-Control-Shift-l" = "mirror horizontal";
            "${mod}-Control-Shift-j" = "mirror vertical";
            "${mod}-Control-Shift-k" = "mirror vertical";

            #"${mod}-a" = "or , and . compare monitors.count = 3 . set_monitors 3440x1440+0+0 3440x1440+3440+0 , set_monitors 3440x1440+0+0 1720x1440+3440+0 1720x1";

            "${mod}-u" = "split bottom 0.5";
            "${mod}-o" = "split right 0.5";
            "${mod}-Control-space" = "split explode";

            "${mod}-Control-h" = "resize left +${resizeStep}";
            "${mod}-Control-j" = "resize down +${resizeStep}";
            "${mod}-Control-k" = "resize up +${resizeStep}";
            "${mod}-Control-l" = "resize right +${resizeStep}";

            "${mod}-period" = "use_index +1 --skip-visible";
            "${mod}-comma" = "use_index -1 --skip-visible";

            "${mod}-BackSpace" = "cycle_monitor";
            "${mod}-Tab" = "cycle_all +1";
            "${mod}-Shift-Tab" = "cycle_all -1";
            "${mod}-c" = "cycle";
            "${mod}-i" = "jumpto urgent";

            "${mod}-r" = "remove";
            "${mod}-s" = "floating toggle";
            "${mod}-f" = "fullscreen toggle";
            "${mod}-Shift-f" = "set_attr clients.focus.floating toggle";
            "${mod}-Shift-m" = "set_attr clients.focus.minimized true";
            "${mod}-Control-m" = "jumpto last-minimized";
            "${mod}-p" = "pseudotile toggle";
            "${mod}-t" = "or , and . compare tags.focus.curframe_wcount = 2 . cycle_layout +1 vertical horizontal max vertical grid , cycle_layout +1";
          }
          // (listToAttrs (concatLists (genList (i: [
            {
              name = "${mod}-${toString (i + 1)}";
              value = "use_index ${toString i}";
            }
            {
              name = "${mod}-Shift-${toString (i + 1)}";
              value = "move_index ${toString i}";
            }
          ]) (length tags))));
        extraConfig =
          ''
            ${pkgs.imagemagick}/bin/convert -flop "${config.stylix.image}" - | feh --bg-fill "${config.stylix.image}" -
            systemctl --user start picom
            systemctl --user start polybar
            pkill '.*glava.*'; glava --desktop &
          ''
          + flatten "${hc} attr " attributes;
      };
    };
  }
