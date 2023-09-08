{ pkgs, lib, config, inputs, ... }: let
INIT_WP = "space.jpg";
hexOpacity =  lib.toHexString ((((builtins.ceil (config.stylix.opacity.desktop * 100)) * 255) / 100));
hyprlandPackage = pkgs.hyprland.overrideAttrs (o: {
    #patches = (o.patches or []) ++ [ ./hyprland.patch ];
});
in
with config.lib.stylix.colors; {
    wayland.windowManager.hyprland = {
        package = hyprlandPackage;
        enable = true;
        xwayland.enable = true;
        enableNvidiaPatches = true;
        settings = {
            input = {
                kb_layout = "ca";
                kb_variant = "multix";
                follow_mouse = true;
                touchpad = {
                    natural_scroll = true;
                    disable_while_typing = true;
                };
            };
            general = {
                sensitivity = 1;
                gaps_in = 10;
                gaps_out = 10;
                border_size = 3;
                "col.inactive_border" = "0x${hexOpacity + base03}";
                "col.active_border" = "0x${hexOpacity + base04}";
                "col.group_border" = "0x${hexOpacity + base0E}";
            };
            decoration = {
                rounding = 20;
                blur = {
                    enabled = true;
                    size = 5;
                    passes = 1;
                };
                drop_shadow = true;
                shadow_range = 30;
                shadow_offset = "5 5";
                shadow_render_power = 8;
                shadow_ignore_window = 0;
                "col.shadow" = "0x77000000";
                #"col.active_border" = "${stylix.opacity + base03}";
                blurls = [ "gtk-layer-shell" "notifications" ];
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
            };
            misc = {
                enable_swallow = true;
                swallow_regex = "^kitty$";
            };
            "$mod" = "SUPER";
            bindm = [
                "$mod,mouse:272,movewindow"
                    "$mod,mouse:273,resizewindow"
            ];
        };
        extraConfig = builtins.concatStringsSep "\n" (builtins.genList (x:
            let xs = builtins.toString x; in ''
                bind=$mod,${xs},workspace,${xs}
                bind=$mod SHIFT,${xs},movetoworkspace,${xs}
            ''
        ) 10) + ''
            exec-once=${pkgs.waybar}/bin/waybar
            exec=${pkgs.swww}/bin/swww init
            exec-once=${pkgs.swww}/bin/swww img ~/.wallpapers/${INIT_WP}

            bind=$mod,Q,exec,${config.programs.swaylock.package}/bin/swaylock
            bind=$mod,B,exec,firefox
            bind=$mod,Return,exec,kitty
            bind=$mod SHIFT,B,exec,firefox --private-window
            bind=$mod,D,exec,wofi --show drun
            bind=$mod,W,exec,looking-glass-client -f /dev/shm/looking-glass1

            bind=$mod SHIFT,Q,exit,
            bind=$mod,S,togglefloating,
            bind=$mod,F,fullscreen,
            bind=$mod SHIFT,F,fakefullscreen,
            bind=$mod,T,togglesplit
            bind=$mod,G,togglegroup
            bind=$mod,U,changegroupactive,f

            bind=$mod,H,movefocus,l
            bind=$mod,L,movefocus,r
            bind=$mod,K,movefocus,u
            bind=$mod,J,movefocus,d

            bind=$mod SHIFT,H,movewindow,l
            bind=$mod SHIFT,L,movewindow,r
            bind=$mod SHIFT,K,movewindow,u
            bind=$mod SHIFT,J,movewindow,d
            bind=$mod SHIFT,C,killactive,

            bind=,XF86AudioRaiseVolume,exec,pactl set-sink-volume @DEFAULT_SINK@ +5%
            bind=,XF86AudioLowerVolume,exec,pactl set-sink-volume @DEFAULT_SINK@ -5%
            bind=,XF86AudioMute,exec,pactl set-sink-mute @DEFAULT_SINK@ toggle
            bind=,XF86MonBrightnessDown,exec, brillo -u 150000 -U 5
            bind=,XF86MonBrightnessUp,exec, brillo -u 150000 -A 5
            bind=,XF86AudioPlay,exec,playerctl play
            bind=,XF86AudioPause,exec,playerctl pause
            bind=,XF86AudioStop,exec,playerctl pause
            bind=,XF86AudioPrev,exec,playerctl previous
            bind=,XF86AudioNext,exec,playerctl next

            monitor=eDP-1,3840x2400@60,0x0,2
            windowrule=opacity 0.93,firefox
            windowrule=opacity 0.85,Element
            windowrule=opacity 0.85,deluge
            windowrule=opacity 0.85,title:Spotify

            env=WLR_DRM_DEVICE,/dev/dri/renderD128

            env=GTK_IM_MODULE,wayland
            env=QT_IM_MODULE,wayland
            env=GLFW_IM_MODULE,wayland
            env=XMODIFIERS,@im=kime
            exec=${pkgs.kime}/bin/kime
        '';
    };
}
