{

  inputs,
  config,
  pkgs,
  lib,
  ...
}:
with lib; {
  imports =
    (mine.autoInclude ./. [
      ./william
    ])
    ++ [
      inputs.impermanence.nixosModules.impermanence
    ];
  options.wallpaper = mkOption {
    type = types.path;
  };
  config = {
    stylix = {
      enable = true;
      image = config.wallpaper;
      polarity = "dark";
      homeManagerIntegration.followSystem = false;
      fonts.sizes = {
        terminal = 48;
        applications = 48;
        desktop = 48;
        popups = 48;
      };
    };

    home-manager = {
      extraSpecialArgs = {
        inherit inputs;
        inherit (config) wallpaper;
      };
      useGlobalPkgs = true;
      useUserPackages = true;
      users.william = import ./william;
      sharedModules = [
        inputs.impermanence.nixosModules.home-manager.impermanence
        inputs.sops-nix.homeManagerModules.sops
        inputs.nixvim.homeManagerModules.nixvim
        inputs.spicetify-nix.homeManagerModules.default
        ../modules/home-manager
      ];
    };

    hardware = {
      enableRedistributableFirmware = true;
      enableAllFirmware = true;
    };

    time.timeZone = "America/Toronto";
    i18n.defaultLocale = "en_CA.UTF-8";
    console = {
      keyMap = "ca";
      earlySetup = true;
      font = "ter-i32b";
      packages = with pkgs; [terminus_font];
    };

    environment = {
      systemPackages = with pkgs; [
        sysstat
        lm_sensors
        vlock
        file
        unzip
        lsof
        lftp
        tmux
        mlocate
        miniupnpc
        acpi
        wget

        # nix-related stuff
        nix-prefetch-github
        nix-output-monitor
        nix-alien
        cntr
      ];
      persistence."/persist" = {
        hideMounts = true;
        directories = [
          "/etc/nixos"
          "/var/log"
          "/var/lib/bluetooth"
          "/var/lib/nixos"
        ];
        files = [
          "/etc/machine-id"
        ];
      };
      shellAliases = {
        "ll" = "ls -alF";
        "la" = "ls -A";
        "l" = "ls -CF";
        "dir" = "dir --color=auto";
        "vdir" = "vdir --color=auto";
        "fgrep" = "fgrep --color=auto";
        "egrep" = "egrep --color=auto";
        "ip" = "ip --color=auto";
        "c" = "clear";
        "ftp" = "lftp";
        "fd" = "fd -H";
        "du" = "du -h";
        "free" = "free -h";
        "df" = "df -h";
        "vim" = "nvim";
        "se" = "command sudo -E -s nvim";
        "s" = ''sudo SSH_AUTH_SOCK="$SSH_AUTH_SOCK" '';
        "sc" = "sudo systemctl";
        "jc" = "sudo journalctl --system";
        "nu" = "sudo nix flake update ";
        "shred" = "shred --remove";
        "cn" = "cd /etc/nixos";
      };
    };

    programs = {
      git = {
        enable = true;
        package = pkgs.gitFull;
        config = {
          include.path = "/home/william/.config/git/config";
          credential.helper = "libsecret";
        };
      };
      fuse.userAllowOther = true;
      neovim = {
        enable = true;
        defaultEditor = true;
        viAlias = true;
        vimAlias = true;
      };
      fish.enable = true;
      nix-ld.enable = true;
    };

    nix = {
      # TODO keep an eye on this
      package = pkgs.nixVersions.git;
      #allowed-users = [ "@wheel" ];
      gc = {
        automatic = true;
        dates = "monthly";
        options = "--delete-older-than 1m";
      };
      optimise = {
        automatic = true;
        dates = ["daily"];
      };
      settings = {
        #auto-optimise-store = true;
        experimental-features = ["nix-command" "flakes"];
        substituters = ["https://hyprland.cachix.org"];
        trusted-public-keys = ["hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="];
      };
    };

    system.stateVersion = "25.11";

    nixpkgs = {
      hostPlatform = lib.mkDefault "x86_64-linux";
      config.joypixels.acceptLicense = true;
      config.allowUnfree = true;
      # For LTO overlay
      #        overlays = [
      #            (final: prev: {
      #                stdenv = prev.stdenvAdapters.addAttrsToDerivation {
      #                NIX_CFLAGS_COMPILE = "-pipe -march=native -O2";
      #                NIX_LDFLAGS = "";
      #                } prev.stdenv;
      #            })
      #        ];
    };
  };
}
