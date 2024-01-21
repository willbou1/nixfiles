{ pkgs, lib, config, inputs, ... }:
with builtins;
with lib;
with config.lib.stylix.colors;
let
    monitors = map (m: "${m.wlrName},${toString m.width}x${toString m.height}@${toString m.rate},${toString m.x}x${toString m.y},${toString (trivial.max m.hScale m.vScale)}") config.home.monitors;
    hexOpacity =  lib.toHexString ((((ceil (config.stylix.opacity.desktop * 100)) * 255) / 100));
    hyprcap = pkgs.writeShellScriptBin "hyprcap" (''
        slurp_args="-b "${withHashtag.base00 + hexOpacity}" -B "${withHashtag.base00 + hexOpacity}" -c "${withHashtag.base0A + hexOpacity}""

        #${pkgs.grim}/bin/grim -l 1 ~/.cache/hyprland/screenfreeze
        #${pkgs.feh}/bin/feh --title screenfreeze ~/.cache/hyprland/screenfreeze &

        [[ "$1" != "monitor" ]] && dims="$(${pkgs.slurp}/bin/slurp $slurp_args -w 3)"
        [[ "$1" == "monitor" ]] && dims="$(${pkgs.slurp}/bin/slurp $slurp_args -o)"

        ${pkgs.grim}/bin/grim -g "$dims" - | ${pkgs.wl-clipboard}/bin/wl-copy

        #${pkgs.killall}/bin/killall feh
    '');
    dic = pkgs.writeShellScriptBin "dic" (''
        qutebrowser --target window 'https://korean.dict.naver.com/koendict/#/main'
        qutebrowser --target tab 'https://koreanhanja.app/'
    '');
    wallpaper = pkgs.writeShellScript "wallpaper.sh" ''
    '';
in {
    home.packages = with pkgs; [
        qt5.qtwayland
        slurp
        grim
        feh
        wl-clipboard
        hyprcap
    ];
    wayland.windowManager.hyprland = {
        package = inputs.hyprland.packages.${pkgs.system}.default;
        enable = true;
        xwayland.enable = true;
        #enableNvidiaPatches = true;
        settings = {
            monitor = monitors ++ [",disable"];
            plugin = {
                touch_gestures = {
                    sensitivity = 1.0;
                    workspace_swipe_fingers = 3;
                    experimental.send_cancel = 0;
                };
            };
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
                workspace_swipe = true;
                workspace_swipe_forever = true;
                workspace_swipe_direction_lock = false;
            };
            general = {
                gaps_in = config.home.gapSize;
                gaps_out = ceil (config.home.gapSize * 1.5);
                sensitivity = 1;
                border_size = config.home.borderSize;
                "col.inactive_border" = lib.mkForce "0x${hexOpacity + base03}";
                "col.active_border" = lib.mkForce "0x${hexOpacity + base0A}";
            };
            group = {
                "col.border_inactive" = lib.mkForce "0x${hexOpacity + base06}";
                "col.border_active" = lib.mkForce "0x${hexOpacity + base0D}";
                "col.border_locked_active" = lib.mkForce "0x${hexOpacity + base06}";
            };
            decoration = {
                rounding = 20;
                blur = {
                    enabled = true;
                    size = 5;
                    passes = 1;
                    special = true;
                };
                drop_shadow = true;
                shadow_range = 30;
                shadow_offset = "5 5";
                shadow_render_power = 8;
                shadow_ignore_window = 0;
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
                special_scale_factor = 0.9;
            };
            misc = {
                enable_swallow = true;
                swallow_regex = "^kitty$";
                disable_hyprland_logo = true;
                allow_session_lock_restore = true;
                background_color = lib.mkForce base00;
                vfr = false;
            };
            "$mod" = "SUPER";
            bindm = [
                "$mod,mouse:272,movewindow"
                "$mod,mouse:273,resizewindow"
            ];
            exec-once = [
                "SVPManager"
                "element-desktop"
                "spotify"
                "qutebrowser"
                "${wallpaper}"
                "${pkgs.swww}/bin/swww init && sleep 2 && ${pkgs.swww}/bin/swww img ${config.stylix.image}"
                # Make sure to clean up after xorg session
                "${pkgs.dbus}/bin/dbus-update-activation-environment --systemd XAUTHORITY XDG_SESSION_ID"
            ];
            windowrule = [
                "idleinhibit fullscreen,.*"

                "opacity 0.93,org.qutebrowser.qutebrowser"
                "opacity 0.93,firefox"

                "workspace 4,mpv"

                "opacity 0.85,Element"
                "workspace 5 silent,Element"

                "opacity 0.85,deluge"

                "opacity 0.85,title:Spotify"
                "workspace 6 silent,title:Spotify"

                "float,SVPManager"
                "workspace 9 silent,SVPManager"
            ];
            bind = [
                "$mod,Q,exec,${config.programs.swaylock.package}/bin/swaylock"
                "$mod,B,exec,qutebrowser"
                "$mod SHIFT,D,exec,${dic}/bin/dic"
                "$mod,Return,exec,MESA_LOADER_DRIVER_OVERRIDE=iris __EGL_VENDOR_LIBRARY_FILENAMES=${pkgs.mesa_drivers}/share/glvnd/egl_vendor.d/50_mesa.json kitty"
                "$mod,N,exec,MESA_LOADER_DRIVER_OVERRIDE=iris __EGL_VENDOR_LIBRARY_FILENAMES=${pkgs.mesa_drivers}/share/glvnd/egl_vendor.d/50_mesa.json kitty ncpamixer"
                "$mod,D,exec,wofi --show drun"
                "$mod,W,exec,looking-glass-client -f /dev/shm/looking-glass1"
                "$mod,C,exec,${hyprcap}/bin/hyprcap"

                "$mod SHIFT,Q,exit,"
                "$mod,S,togglefloating,"
                "$mod,F,fullscreen,"
                "$mod SHIFT,F,fakefullscreen,"
                "$mod,T,togglesplit"
                "$mod,G,togglegroup"
                "$mod,U,changegroupactive,f"

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
                "$mod SHIFT,B,exec,qutebrowser ':open -p'"
            ] ++ concatLists (genList (x :
            let xs = toString (x + 1); in [
                "$mod,${xs},workspace,${xs}"
                "$mod SHIFT,${xs},movetoworkspace,${xs}"
            ]) 9);
        };
    };
}
