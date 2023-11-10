{ ... }:

{
    programs.mpv = {
        config.glsl-shaders = "~/.config/mpv/shaders/film/FSRCNNX_x2_8-0-4-1.glsl:~/.config/mpv/shaders/film/SSimDownscaler.glsl:~/.config/mpv/shaders/film/KrigBilateral.glsl";
    };
    xdg.configFile = {
        "mpv/shaders/film".source = pkgs.fetchFromGitHub {
            owner =  "classicjazz";
            repo =  "mpv-config";
            rev =  "90eebbbc8e1edaab233cafb2412d644cb4c7877d";
            hash =  "sha256-ReXiDCAsWNWBrk48VyNWqSoR2Ic09Zw/TPvWRcG8xvA=";
        } + "/shaders";
    };
}
