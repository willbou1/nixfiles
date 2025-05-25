# local
for input in stylix nixpkgs unstable home-manager nixvim sops-nix impermanence spicetify-nix neovim-nightly-overlay emacs-overlay; do
  cd "/etc/nixos/devel/$input"
  git pull
  cd /etc/nixos
  nix flake update "$input"
done

