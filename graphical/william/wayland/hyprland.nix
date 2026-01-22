{
  pkgs,
  lib,
  config,
  ...
}:
with builtins;
with lib;
with config.lib.stylix.colors; let
  jq = "${pkgs.jq}/bin/jq";
  stylix = config.stylix;
  monitors = map (m: "${m.wlrName},${toString m.width}x${toString m.height}@${toString m.rate},${toString m.x}x${toString m.y},${toString (trivial.max m.hScale m.vScale)}") config.home.monitors;
  hexOpacity = lib.toHexString (((ceil (stylix.opacity.desktop * 100)) * 255) / 100);
  groupFontSize = floor (stylix.fonts.sizes.desktop * 1.2);
  hyprcap = pkgs.writeShellScriptBin "hyprcap" ''
    slurp_args="-b "${withHashtag.base00 + hexOpacity}" -B "${withHashtag.base00 + hexOpacity}" -c "${withHashtag.base0A + hexOpacity}""

    #${pkgs.grim}/bin/grim -l 1 ~/.cache/hyprland/screenfreeze
    #${pkgs.feh}/bin/feh --title screenfreeze ~/.cache/hyprland/screenfreeze &

    [[ "$1" != "monitor" ]] && dims="$(${pkgs.slurp}/bin/slurp $slurp_args -w 3)"
    [[ "$1" == "monitor" ]] && dims="$(${pkgs.slurp}/bin/slurp $slurp_args -o)"

    ${pkgs.grim}/bin/grim -g "$dims" - | ${pkgs.wl-clipboard}/bin/wl-copy

    #${pkgs.killall}/bin/killall feh
  '';
  dic = pkgs.writeShellScriptBin "dic" ''
    qutebrowser --target window 'https://korean.dict.naver.com/koendict/#/main'
    qutebrowser --target tab 'https://koreanhanja.app/'
  '';
in {
  home.packages = with pkgs; [
    qt5.qtwayland
    slurp
    grim
    feh
    wl-clipboard
    hyprcap
    wl-gammarelay-rs
  ];
  xdg.configFile."hypr/xdph.conf".text = ''
    screencopy {
      mas_fps = 30
    }
  '';
  wayland.windowManager.hyprland = {
    enable = true;
    xwayland.enable = true;
    #enableNvidiaPatches = true;
    settings = {
      ecosystem.no_update_news = true;
      monitor = monitors ++ [",disable"];
      input = {
        kb_layout = "ca";
        kb_variant = "multix";
        follow_mouse = true;
        touchpad = {
          natural_scroll = true;
          disable_while_typing = true;
        };
      };
      gestures = {
        workspace_swipe_forever = true;
        workspace_swipe_direction_lock = false;
      };
      general = {
        gaps_in = config.home.gapSize;
        gaps_out = ceil (config.home.gapSize * 1.5);
        border_size = config.home.borderSize;
        "col.inactive_border" = mkForce "0x${hexOpacity + base03}";
        "col.active_border" = mkForce "0x${hexOpacity + base0D}";
      };
      group = {
        "col.border_inactive" = mkForce "0x${hexOpacity + base03}";
        "col.border_active" = mkForce "0x${hexOpacity + base0D}";
        "col.border_locked_active" = mkForce "0x${hexOpacity + base0C}";
        groupbar = rec {
          font_size = groupFontSize;
          font_weight_active = "bold";
          font_weight_inactive = font_weight_active;
          height = floor (groupFontSize * 1.5);
          indicator_height = 0;
          gradients = true;
          gradient_round_only_edges = false;
          gradient_rounding = 20;
          keep_upper_gap = false;
          gaps_in = floor (config.home.gapSize * 0.7);
          gaps_out = gaps_in;
          "col.active" = mkForce "0x${hexOpacity + base0D}";
          "col.inactive" = mkForce "0x${hexOpacity + base03}";
        };
      };
      decoration = {
        rounding = 20;
        blur = {
          enabled = true;
          size = 5;
          passes = 1;
          special = true;
          popups = true;
        };
        shadow = {
          enabled = true;
          range = 30;
          offset = "5 5";
          render_power = 8;
          ignore_window = 0;
        };
        dim_special = 0.4;
      };
      animations = {
        enabled = true;
        animation = [
          "windows,1,7,default"
          "border,1,10,default"
          "borderangle,1,10,default"
          "fade,1,10,default"
          "workspaces,1,6,default"
        ];
      };
      dwindle = {
        pseudotile = true;
        preserve_split = true;
        force_split = 2;
        special_scale_factor = 0.95;
      };
      misc = {
        enable_swallow = true;
        swallow_regex = "^kitty$";
        disable_hyprland_logo = true;
        allow_session_lock_restore = true;
        background_color = mkForce "0x${base00}";
        vfr = false;
        mouse_move_enables_dpms = true;
        key_press_enables_dpms = true;
        font_family = stylix.fonts.monospace.name;
        new_window_takes_over_fullscreen = 2;
      };
      binds.scroll_event_delay = 100;
      "$mod" = "SUPER";
      bindm = [
        "$mod,mouse:272,movewindow"
        "$mod,mouse:273,resizewindow"
      ];
      exec-once = [
        "SVPManager"
        "element-desktop-nightly"
        "spotify"
        "qutebrowser"
        "${pkgs.swww}/bin/swww init"
        # Make sure to clean up after xorg session
        "${pkgs.dbus}/bin/dbus-update-activation-environment --systemd XAUTHORITY XDG_SESSION_ID"
      ];
      windowrule = [
        "idleinhibit fullscreen,class:.*"

        "opacity 0.93 0.93 1.0,class:org.qutebrowser.qutebrowser"
        "opacity 0.93,class:firefox"

        "workspace 4,class:mpv"

        "opacity 0.85,class:Element(-Nightly)*"
        "workspace 5 silent,class:Element(-Nightly)*"

        "opacity 0.85,class:deluge"

        "opacity 0.85 override 0.85 override 0.85 override,workspace:6"
        "workspace 6 silent,title:(.*)(Spotify)(.*)"

        "float,class:SVPManager"
        "workspace 9 silent,class:SVPManager"

        "opacity 0.75,class:udiskie"

        "bordercolor 0x${hexOpacity + base0E},title:private"
      ];
      bind =
        [
          "$mod,B,exec,qutebrowser"
          "$mod SHIFT,D,exec,${dic}/bin/dic"
          "$mod,Return,exec, ${config.home.terminal}"
          "$mod SHIFT,Return,exec, ${config.home.terminal} --title private fish --private -C 'set PRIVATE private && set -x STARSHIP_CONFIG ~/.config/starship_private.toml'"
          "$mod,N,exec,${config.home.terminal} ncpamixer"
          "$mod,E,exec,emacsclient -c"
          "$mod,D,exec,wofi --show drun"
          "$mod,W,exec,looking-glass-client -f /dev/kvmfr0"
          "$mod,C,exec,${hyprcap}/bin/hyprcap"
          "$mod,M,exec,cd /data/datasheets; fd . | wofi -d -i | xargs -I'{}' zathura '/data/datasheets/{}'"

          "$mod SHIFT,Q,exit,"
          "$mod,S,togglefloating,"
          "$mod,F,fullscreen,"
          "$mod SHIFT,F, fullscreenstate, 0 3"
          "$mod,T,togglesplit"
          "$mod,G,togglegroup"
          "$mod,TAB,changegroupactive,f"

          "$mod,H,movefocus,l"
          "$mod,L,movefocus,r"
          "$mod,K,movefocus,u"
          "$mod,J,movefocus,d"

          "$mod,N,swapactiveworkspaces,eDP-1 DP-4"
          "$mod SHIFT,H,movewindow,l"
          "$mod SHIFT,L,movewindow,r"
          "$mod SHIFT,K,movewindow,u"
          "$mod SHIFT,J,movewindow,d"
          "$mod SHIFT,C,killactive,"

          ",XF86AudioRaiseVolume,exec,pactl set-sink-volume @DEFAULT_SINK@ +5%"
          ",XF86AudioLowerVolume,exec,pactl set-sink-volume @DEFAULT_SINK@ -5%"
          ",XF86AudioMute,exec,pactl set-sink-mute @DEFAULT_SINK@ toggle"
          ",XF86AudioPlay,exec,playerctl play-pause"
          ",XF86AudioPause,exec,playerctl pause"
          ",XF86AudioStop,exec,playerctl pause"
          ",XF86AudioPrev,exec,playerctl previous"
          ",XF86AudioNext,exec,playerctl next"

          "$mod SHIFT,S,togglespecialworkspace,secret"
          "$mod SHIFT,B,exec,qutebrowser --restore private ':open -p'"

          "$mod, equal, exec, hyprctl -q keyword cursor:zoom_factor $(hyprctl getoption cursor:zoom_factor -j | ${jq} '.float * 1.2')"
          "$mod, minus, exec, hyprctl -q keyword cursor:zoom_factor $(hyprctl getoption cursor:zoom_factor -j | ${jq} '(.float * 0.8) | if . < 1 then 1 else . end')"
          "$mod, mouse_down, exec, hyprctl -q keyword cursor:zoom_factor $(hyprctl getoption cursor:zoom_factor -j | ${jq} '.float * 1.2')"
          "$mod, mouse_up, exec, hyprctl -q keyword cursor:zoom_factor $(hyprctl getoption cursor:zoom_factor -j | ${jq} '(.float * 0.8) | if . < 1 then 1 else . end')"
          "$mod SHIFT, mouse_up, exec, hyprctl -q keyword cursor:zoom_factor 1"
          "$mod SHIFT, mouse_down, exec, hyprctl -q keyword cursor:zoom_factor 1"
        ]
        ++ concatLists (genList (x: let
            xs = toString (x + 1);
          in [
            "$mod,${xs},workspace,${xs}"
            "$mod SHIFT,${xs},movetoworkspace,${xs}"
          ])
          9);
    };
  };
}
