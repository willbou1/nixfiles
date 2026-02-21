{pkgs, ...}: {
  home.persistence."/persist".directories = [
    ".local/share/keyrings"
  ];
  home.packages = with pkgs; [
    gcr
  ];
  services.gnome-keyring.enable = true;
}
