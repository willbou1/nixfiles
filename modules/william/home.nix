{ config, pkgs, inputs, ... }:

rec {
    imports = [
        inputs.impermanence.nixosModules.home-manager.impermanence
        ./mpv.nix
        ./theme.nix
        ./hyprland.nix
        ./wofi.nix
        ./spotify.nix
        ./services
        ./cli
        ./waybar.nix
        ./xdg.nix
        ./virtualisation.nix
        ./ime.nix
    ];
    home.username = "william";
    home.homeDirectory = "/home/william";
    home.persistence."/persist/home/william" = {
        directories = [
            "priv"
            ".gnupg"
            ".ssh"
            ".mozilla/firefox"
            ".config/Element"
            ".config/spotify"
        ];
        files = [
            ".local/share/fish/fish_history"
        ];
        allowOther = true;
    };
    home.file.".home-manager".source =
        config.lib.file.mkOutOfStoreSymlink "/etc/nixos/modules/william";

    home.packages = with pkgs; [
        libreoffice-fresh
        vieb
        inputs.nixos-stable.legacyPackages.x86_64-linux.element-desktop
        gimp
        playerctl
        hyprpaper
        swww
        dconf

        # Fonts
        baekmuk-ttf
        nerdfonts
    ];

    fonts.fontconfig.enable = true;

    home.stateVersion = "23.05";

    programs = {
        firefox.enable = true;
        
        kitty = import ./kitty.nix;
        zathura.enable = true;

        home-manager.enable = true;

    };

    # Wallpapers
    home.file.".wallpapers".source = ../../resources/wallpapers;
}
