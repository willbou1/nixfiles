{
  pkgs,
  lib,
  ...
}:
with lib; {
  imports = mine.autoInclude ./. [];

  home.persistence."/persist/home/william".directories = [
    ".local/share/zoxide"
    ".config/qalculate"
  ];

  programs = {
    ssh = {
      enable = true;
      enableDefaultConfig = false;
      matchBlocks."*" = {
        forwardAgent = false;
        addKeysToAgent = "no";
        compression = false;
        serverAliveInterval = 0;
        serverAliveCountMax = 3;
        hashKnownHosts = false;
        userKnownHostsFile = "~/.ssh/known_hosts";
        controlMaster = "no";
        controlPath = "~/.ssh/master-%r@%n:%p";
        controlPersist = "no";
      };
      extraConfig = "AddKeysToAgent yes";
    };
    git = {
      enable = true;
      settings.user = {
        name = "William Boulanger";
        email = "willbou2@gmail.com";
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

    weechat
    aspell
    aspellDicts.en
    aspellDicts.fr
  ];
}
