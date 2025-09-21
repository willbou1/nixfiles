# local
cd /etc/nixos
for input in stylix nixpkgs unstable home-manager nixvim sops-nix impermanence spicetify-nix neovim-nightly-overlay emacs-overlay; do
  nix flake lock --update-input "$input"
done

