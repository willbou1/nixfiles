{pkgs, ...}: {
  home.persistence."/persist/home/william".directories = [
    ".cache/nix-index"
  ];
  home.packages = with pkgs; [pkgs.nix-index];
}
