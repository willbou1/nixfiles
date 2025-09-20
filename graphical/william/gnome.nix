{pkgs, ...}: {
  home.persistence."/persist/home/william".directories = [
    ".local/share/keyrings"
  ];
  home.packages = with pkgs; [
    gcr
  ];
  services.gnome-keyring.enable = true;
}
