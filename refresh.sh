#! /usr/bin/env nix-shell
#! nix-shell -i bash -p bash

for input in impermanence neovim-nightly-overlay stylix home-manager nixvim \
    spicetify-nix sops-nix nixpkgs unstable nur; do
    sudo nix flake lock --update-input $input
done
