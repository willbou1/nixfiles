{
  pkgs,
  config,
  ...
}: let
  hyprrotate = pkgs.writeShellScriptBin "hyprrotate" ''
    #! /bin/sh
    transform="$(${pkgs.hyprland}/bin/hyprctl -j monitors | ${pkgs.jq}/bin/jq '.[0].transform')"
    if [ "$transform" -eq "0" ]; then
        ${pkgs.hyprland}/bin/hyprctl --batch "keyword monitor eDP-1,preferred,auto,auto,transform,1; keyword input:touchdevice:transform 1;"
    else
        ${pkgs.hyprland}/bin/hyprctl --batch "keyword monitor eDP-1,preferred,auto,auto,transform,0; keyword input:touchdevice:transform 0;"
    fi
    sleep 0.2
    ${pkgs.swww}/bin/swww img ${config.stylix.image}
  '';
  wallpaper = pkgs.writeShellScript "wallpaper.sh" ''
    sleep 3
    ${pkgs.swww}/bin/swww img ${config.stylix.image}
    last_workspace_id=100
    while true; do
        workspace_id="$(hyprctl activeworkspace -j | ${pkgs.jq}/bin/jq '.id')"
        # If Hyprland is not running
        [[ $workspace_id -eq *"HYPRLAND_INSTANCE_SIGNATURE"* ]] && break

        if [ $workspace_id -ne $last_workspace_id ]; then
            last_workspace_id="$workspace_id"
            [ $workspace_id -eq 8 ] && ${pkgs.swww}/bin/swww img ${../../../resources/wallpapers/gil_nak_won_3d.png} || ${pkgs.swww}/bin/swww img ${config.stylix.image}
        fi
    done
  '';
  dic = pkgs.writeShellScriptBin "dic" ''
    qutebrowser --target window 'https://korean.dict.naver.com/koendict/#/main'
    qutebrowser --target tab 'https://koreanhanja.app/'
  '';
in {
  home.packages = with pkgs; [
    hyprrotate
  ];
  wayland.windowManager.hyprland = {
    #lugins = [ pkgs.hyprlandPlugins.hyprgrass ];
    settings = {
      env = [
        "WLR_DRM_DEVICE,/dev/dri/by-path/pci-0000:00:02.0-card"
      ];
      exec-once = [
        "${pkgs.waybar}/bin/waybar"
        #"${wallpaper}"
      ];
      bind = [
        "$mod,M,exec,${hyprrotate}/bin/hyprrotate"
        ",XF86MonBrightnessDown,exec, ${pkgs.brillo}/bin/brillo -u 150000 -U 5"
        ",XF86MonBrightnessUp,exec, ${pkgs.brillo}/bin/brillo -u 150000 -A 5"
      ];
    };
    extraConfig = ''
      device {
          name = elan2097:00-04f3:2a15
          transform = 0
          output = eDP-1
      }
      device {
          name = sof-soundwire-headset-jack
          enabled = false
      }
    '';
  };
}
