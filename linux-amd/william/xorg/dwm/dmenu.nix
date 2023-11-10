{ config, pkgs, ... }:
with builtins;
with config.lib.stylix.colors.withHashtag;
let
    fonts = config.stylix.fonts;
    alpha = toString (ceil (255 * config.stylix.opacity.desktop));
    myDmenu = pkgs.dmenu.overrideAttrs (prev: {
        src = pkgs.fetchFromGitHub {
            owner = "willbou1";
            repo = "dmenu";
            rev = "78ff93302b4c7cbca7792dd55c49a5228e4751c1";
            hash = "sha256-naowy3DuodhLlfkvwK5iRak29X088uifowlXnOpYlJM=";
        };
        postPatch = let
            configFile = pkgs.writeText "config.def.h" ''
static int fuzzy = 1;                      /* -F  option; if 0, dmenu doesn't use fuzzy matching     */
static double opacity = 1.0;                /* -o  option; defines alpha translucency        */
static int topbar = 1;                      /* -b  option; if 0, dmenu appears at bottom     */
static int centered = 1;                    /* -c option; centers dmenu on screen */
static int min_width = 500;                    /* minimum width when centered */
static int colorprompt = 0;                /* -p  option; if 1, prompt uses SchemeSel, otherwise SchemeNorm */
static int centertext = 0;
static int closeNoLines = 1;
static const unsigned int border_width = 5;
/* -fn option overrides fonts[0]; default X11 font or font set */
static const char *fonts[] = {
    "${fonts.monospace.name}:size=${toString fonts.sizes.applications}",
    "${fonts.emoji.name}:pixelsize=${toString fonts.sizes.applications}:antialias=true:autohint=true"
};
static const char *prompt      = ":";      /* -p  option; prompt to the left of input field */
static const char *colors[SchemeLast][2] = {
	/*                  fg         bg       */
	[SchemeNorm] = { "${base05}", "${base00}" },
	[SchemeSel] = { "${base00}", "${base03}" },
	[SchemeOut] = { "${base05}", "${base00}" },
};
static const unsigned int alphas[][3]      = {
	/*               fg      bg        border     */
	[SchemeNorm] = { OPAQUE, ${alpha}, ${alpha} },
	[SchemeSel]  = { OPAQUE, OPAQUE, ${alpha} },
};
/* -l option; if nonzero, dmenu uses vertical list with given number of lines */
static unsigned int lines      = 8;

/*
 * Characters not considered part of a word while deleting words
 * for example: " /?\"&[]"
 */
static const char worddelimiters[] = " ";
            '';
            in prev.postPatch + "cp ${configFile} config.def.h";
    });
in {
    home.packages = with pkgs; [
        myDmenu
        j4-dmenu-desktop
    ];
}
