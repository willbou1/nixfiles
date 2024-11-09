{
  lib,
  config,
  pkgs,
  inputs,
  ...
}: {
  options.i18n.inputMethod.kime.translationLayer = lib.mkOption {
    type = lib.types.str;
    default = "null";
  };
  config = {
    home = {
      packages = with pkgs; [
        kime
      ];
      sessionVariables = {
        GTK_IM_MODULE = "kime";
        QT_IM_MODULE = "kime";
        DefaultIMModule = "kime";
        XMODIFIERS = "@im=kime";
        XMODIFIER = "@im=kime";
      };
    };
    wayland.windowManager.hyprland.settings.exec = ["kime"];
    xsession.windowManager.herbstluftwm.extraConfig = "kime";
    xdg.configFile."kime/config.yaml".text = ''
      engine:
        translation_layer: ${config.i18n.inputMethod.kime.translationLayer}
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
  };
}
