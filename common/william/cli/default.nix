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
    btop.enable = true;
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
    yt-dlp
    qpdf
    p7zip
    ncpamixer
    lolcat
    progress
    fd
    nvtopPackages.full
    ani-cli
    mkvtoolnix

    weechat
    aspell
    aspellDicts.en
    aspellDicts.fr
  ];
}
