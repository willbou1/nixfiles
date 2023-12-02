{ pkgs, lib, config, inputs, ... }: let
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
dic = pkgs.writeShellScriptBin "dic" (''
    qutebrowser --target window 'https://korean.dict.naver.com/koendict/#/main'
    qutebrowser --target tab 'https://koreanhanja.app/'
'');
in {
    home.packages = with pkgs; [
        hyprrotate
    ];
    wayland.windowManager.hyprland = {
            plugins = [
            inputs.hyprgrass.packages.${pkgs.system}.default
        ];
		settings = {
		"device:elan2097:00-04f3:2a15" = {
		    transform = 0;
		    output = "eDP-1";
		};
		"device:sof-soundwire-headset-jack".enabled = false;
		env = [
		    "WLR_DRM_DEVICE,/dev/dri/by-path/pci-0000:00:02.0-card"
		];
		exec-once = [
		    "${pkgs.waybar}/bin/waybar"
		];
		bind = [
		    "$mod,M,exec,${hyprrotate}/bin/hyprrotate"
		    ",XF86MonBrightnessDown,exec, ${pkgs.brillo}/bin/brillo -u 150000 -U 5"
		    ",XF86MonBrightnessUp,exec, ${pkgs.brillo}/bin/brillo -u 150000 -A 5"
		];
		};
    };
}
