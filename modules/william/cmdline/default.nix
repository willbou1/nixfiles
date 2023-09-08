{ pkgs, ... }:

{
    imports = [
        ./lsd.nix
        ./fish.nix
        ./starship.nix
    ];

    programs = {
        btop = {
            enable = true;
            settings.color_theme = "wpgtk";
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
        ncpamixer
        vifm-full
        fastfetch
        lolcat
        progress
        fd

        weechat
        aspell
        aspellDicts.en
        aspellDicts.fr
    ];

    xdg.configFile."fastfetch".source = ./fastfetch;
}
