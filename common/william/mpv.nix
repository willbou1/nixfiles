{ inputs, pkgs, ... }: let
mpv-unwrapped = pkgs.mpv-unwrapped.override {
    vapoursynthSupport = true;
    ffmpeg_5 = pkgs.ffmpeg_5-full;
};
mpv = pkgs.wrapMpv mpv-unwrapped {
    extraMakeWrapperArgs = [
        "--prefix" "LD_LIBRARY_PATH" ":" "${pkgs.vapoursynth-mvtools}/lib/vapoursynth"
    ];
    scripts = [
        pkgs.mpvScripts.uosc
        pkgs.mpvScripts.mpris
        pkgs.mpvScripts.thumbfast
        pkgs.mpvScripts.sponsorblock
    ];
};
in {
    programs.mpv = {
        enable = true;
        package = mpv;
        config = {
            slang = "kor,ko,eng,en,enUS";
            sub-font-size = 75;
            sub-margin-y = 50;
            sub-back-color = "0.0/0.0/0.0/0.45";
            sub-border-size = 0;
            demuxer-mkv-subtitle-preroll = true;
            subs-with-matching-audio = false;

            osc = false;
            osd-bar = false;
            border = false;
            video-sync = "display-resample";

            alang = "jpn,jp,kor,ko,eng,en";
            af = "acompressor=ratio=4,loudnorm";

            window-scale = 1;
            load-osd-console = true;
            osd-duration = 2000;

            profile = "gpu-hq";
            hwdec = "auto-copy";
            hwdec-codecs = "all";
            dscale = "mitchell";
            cscale = "spline64";
            glsl-shaders = "~/.config/mpv/shaders/film/FSRCNNX_x2_8-0-4-1.glsl:~/.config/mpv/shaders/film/SSimDownscaler.glsl:~/.config/mpv/shaders/film/KrigBilateral.glsl";
            #glsl-shaders = "~/.config/mpv/shaders/film/FSRCNNX_x2_8-0-4-1.glsl:~/.config/mpv/shaders/film/SSimDownscaler.glsl";
            input-ipc-server = "/tmp/mpvsocket";
            hr-seek-framedrop = false;
            resume-playback = false;
        };
        profiles = {
            pause_on_focus_lost = {
                profile-cond = "not focused";
                profile-restore = "copy-equal";
                pause = true;
            };
            language_learning.speed = "0.85";
        };
        bindings = {
            "ALT+k" = "add sub-scale +0.1";
            "ALT+j" = "add sub-scale -0.1";
            "CTRL+0" = "no-osd change-list glsl-shaders set \"\"";
        };
    };

    # Get MPV shaders
    xdg.configFile = {
        "mpv/shaders/anime4k".source = pkgs.fetchFromGitHub {
            owner = "bloc97";
            repo = "Anime4K";
            rev = "8e39551ce96ed172605c89b7dd8be855b5502cc9";
            hash = "sha256-01js/vr+kpFAm2Hfj64ad+odZKYiC9TZiTTj6mwAFd8=";
        } + "/glsl";
        "mpv/shaders/film".source = pkgs.fetchFromGitHub {
            owner =  "classicjazz";
            repo =  "mpv-config";
            rev =  "90eebbbc8e1edaab233cafb2412d644cb4c7877d";
            hash =  "sha256-ReXiDCAsWNWBrk48VyNWqSoR2Ic09Zw/TPvWRcG8xvA=";
        } + "/shaders";
    };
}
