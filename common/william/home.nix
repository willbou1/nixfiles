{ config, pkgs, inputs, ... }:

rec {
    imports = [
        ./mpv.nix
        ./theme.nix
        ./hyprland.nix
        ./wofi.nix
        ./spotify.nix
        ./services
        ./cli
        ./xdg.nix
        ./virtualisation.nix
        ./zathura.nix
        ./ime.nix
        ./qutebrowser.nix
    ];
    home.username = "william";
    home.homeDirectory = "/home/william";
    sops = {
        defaultSopsFile = ./secrets.yaml;
        age.sshKeyPaths = [ "/home/william/.ssh/id_ed25519" ];
    };
    home.persistence."/persist/home/william" = {
        directories = [
            ".gnupg"
            ".ssh"
            ".mozilla/firefox"
            ".config/Element"
        ];
        allowOther = true;
    };
    home.file.".home-manager".source =
        config.lib.file.mkOutOfStoreSymlink "/etc/nixos/modules/william";

    home.packages = with pkgs; [
        home-manager
        libreoffice-fresh
        element-desktop
        gimp
        playerctl
        hyprpaper
        swww
        dconf
        helvum

        # Fonts
        baekmuk-ttf
        nerdfonts
    ];

    fonts.fontconfig.enable = true;

    home.stateVersion = "23.05";

    programs = {
        firefox.enable = true;
        
        kitty = import ./kitty.nix;
    };

    # Wallpapers
    home.file.".wallpapers".source = ../../resources/wallpapers;
}
