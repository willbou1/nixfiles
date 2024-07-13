{ pkgs, ... }:

{
    services.emacs = {
        enable = true;
        client.enable = true;
    };

    home.packages = with pkgs; [ emacs ];

    home.file.".emacs.d".source = pkgs.fetchFromGitHub {
        owner = "doomemacs";
        repo = "doomemacs";
        rev = "517daa4ed9168855c202ba2fd28920f6ee17249f";
        hash = "sha256-fW+TA5AR9xwRhFHLB2frH3MGlZuL18aRQleg55XGqwA=";
    };

    systemd.user.sessionVariables = {
        DOOMLOCALDIR = "$HOME/.local/share/doomemacs";
        DOOMPROFILELOADFILE = "$HOME/.local/share/doomemacs/profiles/load.el";
    };
}
