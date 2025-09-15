{pkgs, ...}: {
  home.persistence."/persist/home/william".directories = [
    #".config/fcitx5"
  ];
  i18n.inputMethod = {
    enable = true;
    type = "fcitx5";
    fcitx5 = {
      settings = {
        globalOptions = {
          "Hotkey/TriggerKeys" = {
            "0" = "Super+space";
          };
        };
        inputMethod = {
          "Groups/0" = {
            Name = "Default";
            "Default Layout" = "us";
            "DefaultIM" = "hangul";
          };
          "Groups/0/Items/0" = {
            Name = "keyboard-us";
            Layout = "";
          };
          "Groups/0/Items/1" = {
            Name = "hangul";
            Layout = "";
          };
          "Groups/0/Items/2" = {
            Name = "mozc";
            Layout = "";
          };
          GroupOrder = {
            "0" = "Default";
          };
        };
       };
      addons = with pkgs; [
        fcitx5-mozc
        fcitx5-gtk
        fcitx5-hangul
        fcitx5-chinese-addons
      ];
    };
  };
}
