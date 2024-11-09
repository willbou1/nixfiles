{
  lib,
  inputs,
  config,
  pkgs,
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

  hardware.enableRedistributableFirmware = true;
  hardware.enableAllFirmware = true;

  time.timeZone = "America/Toronto";
  i18n.defaultLocale = "en_CA.UTF-8";
  console.keyMap = "ca";
  console.earlySetup = true;
  console.font = "ter-i32b";
  console.packages = with pkgs; [terminus_font];

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
    etc = {
      "lftp.conf" = {
        text = "set ssl:verify-certificate no";
        mode = "0644";
      };
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
      "nu" = "sudo nix flake update '/etc/nixos?submodules=1'";
      "shred" = "shred --remove";
    };
  };

  programs = {
    git = {
      enable = true;
      config = {
        include.path = "/home/william/.config/git/config";
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
    package = pkgs.nixFlakes;
    #allowed-users = [ "@wheel" ];
    gc = {
      automatic = true;
      dates = "monthly";
      options = "--delete-older-than 1m";
    };
    settings = {
      auto-optimise-store = true;
      experimental-features = ["nix-command" "flakes"];
      substituters = ["https://hyprland.cachix.org"];
      trusted-public-keys = ["hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="];
    };
  };

  system.stateVersion = "24.05";

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
}
