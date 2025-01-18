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
    nvtopPackages.full
    ani-cli
    mkvtoolnix
  ];
}
