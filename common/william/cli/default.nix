{
  pkgs,
  lib,
  ...
}:
with lib;
{
  imports = mine.autoInclude ./. [];

  home.persistence."/persist" = {
    directories = [
      ".local/share/zoxide"
    ];
  };

  programs = {
    ssh = {
      enable = true;
      enableDefaultConfig = false;
      matchBlocks = {
        "*" = {
          forwardAgent = false;
          addKeysToAgent = "no";
          compression = false;
          serverAliveInterval = 30;
          serverAliveCountMax = 3;
          hashKnownHosts = false;
          userKnownHostsFile = "~/.ssh/known_hosts";
          controlMaster = "auto";
          controlPath = "~/.ssh/control-%C";
          controlPersist = "600";
        };
        "ourmiraculous" = {
          hostname = "ourmiraculous.com";
          port = 2223;
          user = "william";
        };
      };
      extraConfig = "AddKeysToAgent yes";
    };
    git = {
      enable = true;
      settings = {
        user = {
          name = "William Boulanger";
          email = "willbou2@gmail.com";
        };
        safe.directory = [
          "/etc/nixos"
        ];
      };
    };
    btop = {
      enable = true;
      settings.vim_keys = true;
    };
    bat = {
      enable = true;
      config = {
        italic-text = "always";
      };
    };
    ripgrep.enable = true;
    fzf.enable = true;
    zoxide = {
      enable = true;
      enableFishIntegration = true;
    };
    qalc = {
      enable = true;
      config = {
        General = {
          imaginary_j = true;
          digit_grouping = true;
        };
        Mode = {
          calculate_as_you_type = true;
        };
      };
      functions = {
        "RC" = "1 / (2 * pi * \\x ohms * \\y farads)";
      };
    };
  };

  home.packages = with pkgs; [
    p7zip
    lolcat
    progress
    fd
    bash-language-server

    weechat
    aspell
    aspellDicts.en
    aspellDicts.fr
  ];
}
