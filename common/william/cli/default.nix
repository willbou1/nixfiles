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
    home.persistence."/persist/home/william".directories = [
        ".local/share/zoxide"
    ];

    programs = {
        ssh = {
            enable = true;
            extraConfig = "AddKeysToAgent yes";
        };
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
    };

    home.packages = with pkgs; [
        yt-dlp
        qpdf
        p7zip
        ncpamixer
        fastfetch
        lolcat
        progress
        fd
        nvtop
        ani-cli
        mkvtoolnix

        weechat
        aspell
        aspellDicts.en
        aspellDicts.fr
    ];

    xdg.configFile."fastfetch".source = ./fastfetch;
}
