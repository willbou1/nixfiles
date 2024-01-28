{ lib, config, pkgs, inputs, ... }:
with lib;
{
    imports = [
        ./element.nix
        ./nix.nix
        ./mpv.nix
        ./theme.nix
        ./spotify.nix
        ./cli
        ./xorg
        ./xdg.nix
        ./virtualisation.nix
        ./zathura.nix
        ./qutebrowser
        ./dunst.nix
        ./kitty.nix
        ./audio.nix
        ./ime.nix
        ./wayland
        ./minecraft.nix
    ];
    options.home = {
        terminal = mkOption {
            type = types.str;
        };
        monitors = mkOption {
            type = (with types; listOf (attrsOf (oneOf [float int str])));
        };
        gapSize = mkOption {
            type = types.int;
        };
        borderSize = mkOption {
            type = types.int;
        };
    };
    config = {
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
            gimp
            playerctl
            hyprpaper
            swww
            dconf
            helvum
            steam
            wally-cli
        ];

        home.stateVersion = "23.05";

        programs = {
            firefox.enable = true;
        };

        services = {
            udiskie = {
                enable = true;
                tray = "never";
            };
            ssh-agent.enable = true;
            easyeffects.enable = true;
        };

        # default to aptx_ll for my bluetooth headphones
        home.file.".local/state/wireplumber/default-profile".text = ''
            [default-profile]
            bluez_card.00_1B_66_C0_52_BD=a2dp-sink-aptx_ll
        '';
        dconf.settings = {
            "org/virt-manager/virt-manager/connections" = {
                autoconnect = [ "qemu:///system" ];
                uris = [ "qemu:///system" ];
            };
            "org/virt-manager/virt-manager/urls" = {
                isos = [ "/home/william/priv/downloads/" ];
            };
            "org/virt-manager/virt-manager/confirm" = {
                forcepoweroff = false;
            };
            "org/virt-manager/virt-manager" = {
                xmleditor-enabled = true;
            };
        };
    };
}
