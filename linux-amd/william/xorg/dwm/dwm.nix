{ pkgs, config, ... }:
with config.lib.stylix.colors.withHashtag;
with builtins;
let
    fonts = config.stylix.fonts;
    alpha = toString (ceil (255 * config.stylix.opacity.desktop));
    myDwm = (pkgs.dwm.overrideAttrs (prev: {
        buildInputs = (prev.buildInputs or []) ++ (with pkgs.xorg; [
            xorgproto libXext
        ]);
        src = pkgs.fetchFromGitHub {
            owner = "willbou1";
            repo = "dwm";
            rev = "fb44c1159a2aafb6b3ebf3933878246bf3e001b0";
            hash = "sha256-MQFjM/tY+PDFigbyEakk8n2AyGynlURbEQxKHUxLKMc=";
        };
    })).override {
        conf = ''
static const char *fonts[] = {
    "${fonts.monospace.name}:size=${toString fonts.sizes.desktop}",
    "${fonts.emoji.name}:pixelsize=${toString fonts.sizes.desktop}:antialias=true:autohint=true"
};
static const char *colors[][3]      = {
	/*               fg         bg         border   */
	[SchemeNorm] = { "${base05}", "${base00}", "${base03}" },
	[SchemeSel]  = { "${base05}", "${base01}",  "${base0A}"  },
};
static const unsigned int alphas[][3]      = {
	/*               fg      bg        border     */
	[SchemeNorm] = { OPAQUE, ${alpha}, ${alpha} },
	[SchemeSel]  = { OPAQUE, ${alpha}, ${alpha} },
};
static const char *termcmd[]  = { "${config.home.terminal}", NULL };
static const char *dmenucmd[] = { "j4-dmenu-desktop", "--dmenu=dmenu", "--term=${config.home.terminal}", "--display-binary", "--use-xdg-de", NULL };
static const char *sdmenucmd[] = { "sudo", "-A", "j4-dmenu-desktop", "--dmenu=dmenu", "--term=${config.home.terminal}", "--display-binary", "--use-xdg-de", NULL };
        '' + (readFile ./config.h);
    };
in {
    home.packages = with pkgs; [
        myDwm
        xwinwrap
    ];
    xdg.configFile = {
        "dwm/autostart.sh".source =
        pkgs.writeShellScript "autostart.sh "''
            systemctl --user restart picom
            ${pkgs.dwmblocks}/bin/dwmblocks &
            ${pkgs.imagemagick}/bin/convert -flop "${config.stylix.image}" - | feh --bg-fill "${config.stylix.image}" -
        '';
        "dwm/autostart_blocking.sh".source =

        pkgs.writeShellScript "autostart_blocking.sh" ''

        '';
    };
}

