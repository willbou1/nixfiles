{ config, pkgs, inputs, ... }:

rec {
    imports = [
        inputs.impermanence.nixosModules.home-manager.impermanence
        inputs.stylix.homeManagerModules.stylix
        ./mpv.nix
        ./theme.nix
        ./hyprland.nix
        ./wofi.nix
        ./spotify.nix
        ./services
        ./nvim.nix
        ./cmdline
        ./waybar.nix
    ];
    home.username = "william";
    home.homeDirectory = "/home/william";
    xdg = {
        enable = true;
        userDirs = {
            music = "${config.home.homeDirectory}/priv/music";
            pictures = "${config.home.homeDirectory}/priv/pics";
            videos = "${config.home.homeDirectory}/priv/vids";
            download = "${config.home.homeDirectory}/priv/dwl";
        };
    };
    home.persistence."/persist/home/william" = {
        directories = [
            "priv"
            ".gnupg"
            ".ssh"
            ".mozilla/firefox"
            ".config/Element"
            ".config/spotify"
            #".local/share/keyrings"
            #".local/share/direnv"
        ];
        files = [
            ".local/share/fish/fish_history"
        ];
        allowOther = true;
    };
    home.file.".home-manager".source =
        config.lib.file.mkOutOfStoreSymlink "/etc/nixos/modules/william";

    home.packages = with pkgs; [
        vieb
        element-desktop
        gimp
        kime
        playerctl
        hyprpaper
        svp
        swww
        dconf

        # Fonts
        baekmuk-ttf
        #nerdfonts
    ];

    fonts.fontconfig.enable = true;

    home.stateVersion = "23.05";

    programs = {
        firefox.enable = true;
        
        kitty = import ./kitty.nix;
        zathura.enable = true;

        home-manager.enable = true;

        git = {
            enable = true;
            userName = "William Boulanger";
            userEmail = "willbou2@gmail.com";
        };
    };

    # Wallpapers
    home.file.".wallpapers".source = ../wallpapers;
}
