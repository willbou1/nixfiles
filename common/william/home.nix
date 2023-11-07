{ config, pkgs, inputs, ... }:

rec {
    imports = [
        ./mpv.nix
        ./theme.nix
        ./spotify.nix
        ./cli
        ./xdg.nix
        ./virtualisation.nix
        ./zathura.nix
        ./ime.nix
        ./qutebrowser.nix
        ./dunst.nix
        ./wayland
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
        files = [
            ".config/deluge/hostlist.conf"
            ".config/deluge/gtk3ui.conf"
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

    services = {
        udiskie = {
            enable = true;
            tray = "never";
        };
        ssh-agent.enable = true;
        easyeffects.enable = true;
    };

    # Wallpapers
    home.file.".wallpapers".source = ../../resources/wallpapers;
}
