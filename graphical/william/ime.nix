{pkgs, ...}: {
  home.persistence."/persist/home/william".directories = [
    ".config/fcitx5"
  ];
  i18n.inputMethod = {
    enabled = "fcitx5";
    fcitx5.addons = with pkgs; [
      fcitx5-mozc
      fcitx5-gtk
      fcitx5-hangul
      fcitx5-chinese-addons
    ];
  };
}
