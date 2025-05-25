{pkgs, ...}: {
  programs.fish = {
    shellAliases = {
      "lo" = "libreoffice";
      "so" = "soffice";
      "d2p" = "soffice --headless --convert-to pdf";
    };
  };
}
