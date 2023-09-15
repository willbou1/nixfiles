{ pkgs, ... }:

{
    imports = [
        ./vifm.nix
        ./nvim.nix
        ./lsd.nix
        ./fish.nix
        ./starship.nix
        ./mail.nix
    ];

    programs = {
        git = {
            enable = true;
            userName = "William Boulanger";
            userEmail = "willbou2@gmail.com";
        };
        btop = {
            enable = true;
            settings.color_theme = "TTY";
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
        neomutt.enable = true;
    };

    home.packages = with pkgs; [
        p7zip
        ncpamixer
        fastfetch
        lolcat
        progress
        fd
        nvtop
        ani-cli

        weechat
        aspell
        aspellDicts.en
        aspellDicts.fr
    ];

    xdg.configFile."fastfetch".source = ./fastfetch;
}
