{ config, pkgs, inputs, ... }:

rec {
    home.packages = with pkgs; [
        kime
    ];
    wayland.windowManager.hyprland.settings = {
        env = [
            "GTK_IM_MODULE,kime"
            "QT_IM_MODULE,kime"
            "DefaultIMModule,kime"
            "XMODIFIERS,@im=kime"
            "XMODIFIER,@im=kime"
        ];
        exec = [ "kime" ];
    };
    xdg.configFile."kime/config.yaml".text = ''
engine:
  global_hotkeys:
    AltR:
      behavior: Ignore
      result: Bypass
    Super-Space:
      behavior: !Toggle
      - Hangul
      - Latin
      result: Consume
'';
}
