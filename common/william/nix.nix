{pkgs, ...}: {
  home.persistence."/persist".directories = [
    ".cache/nix-index"
  ];
  home.packages = with pkgs; [pkgs.nix-index];
}
