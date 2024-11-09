{
  pkgs,
  lib,
  ...
}:
with lib; {
  imports = mine.autoInclude ./. [];

  home.packages = with pkgs; [
    yt-dlp
    qpdf
    ncpamixer
    nvtopPackages.full
    ani-cli
    mkvtoolnix

    weechat
    aspell
    aspellDicts.en
    aspellDicts.fr
  ];
}
