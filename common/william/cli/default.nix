{
  pkgs,
  lib,
  ...
}:
with lib; {
  imports = mine.autoInclude ./. [];

  home.persistence."/persist/home/william".directories = [
    ".local/share/zoxide"
  ];

  programs = {
    ssh = {
      enable = true;
      extraConfig = "AddKeysToAgent yes";
    };
    git = {
      enable = true;
      userName = "William Boulanger";
      userEmail = "willbou2@gmail.com";
    };
    btop = {
      enable = true;
      settings.vim_keys = true;
    };
    bat = {
      enable = true;
      config = {
        italic-text = "always";
      };
    };
    ripgrep.enable = true;
    fzf.enable = true;
    zoxide = {
      enable = true;
      enableFishIntegration = true;
    };
  };

  home.packages = with pkgs; [
    p7zip
    lolcat
    progress
    fd

    weechat
    aspell
    aspellDicts.en
    aspellDicts.fr
  ];
}
