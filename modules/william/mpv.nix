{ pkgs, ... }:
{
    programs.mpv = {
        enable = true;
        config = {
            slang = "kor,ko,eng,en,enUS";
            sub-font-size = 75;
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
            dscale = "mitchell";
            cscale = "spline64";
            glsl-shader = "~~/shaders/aiupscale/Photo/2x/AiUpscale_HQ_Sharp_2x2Photo.glsl";
        };
        profiles = {
            pause_on_focus_lost = {
                profile-cond = "not focused";
                profile-restore = "copy-equal";
                pause = true;
            };
            svp = {
                input-ipc-server = "/tmp/mpvsocket";
                hr-seek-framedrop = false;
                resume-playback = false;
            };
            language_learning.speed = "0.85";
        };
        bindings = {
            "ALT+k" = "add sub-scale +0.1";
            "ALT+j" = "add sub-scale -0.1";

            "CTRL+1" = "no-osd change-list glsl-shaders set \"~~/shaders/aiupscale/Photo/2x/AiUpscale_HQ_Sharp_2x_Photo.glsl\"; show-text \"AiUpscale: 2x HQ Sharp\"";
            "CTRL+2" = "no-osd change-list glsl-shaders set \"~~/shaders/aiupscale/Photo/2x/AiUpscale_HQ_2x_Photo.glsl\"; show-text \"AiUpscale: 2x HQ\"";
            "CTRL+3" = "no-osd change-list glsl-shaders set \"~~/shaders/aiupscale/Photo/2x/AiUpscale_HQ_Sharp_4x_Photo.glsl\"; show-text \"AiUpscale: 4x HQ Sharp\"";
            "CTRL+4" = "no-osd change-list glsl-shaders set \"~~/shaders/aiupscale/Photo/2x/AiUpscale_HQ_4x_Photo.glsl\"; show-text \"AiUpscale: 4x HQ\"";
        };
        scripts = [
            pkgs.mpvScripts.uosc
            pkgs.mpvScripts.mpris
            pkgs.mpvScripts.thumbfast
            pkgs.mpvScripts.sponsorblock
        ];
    };

    # Get MPV shaders
    xdg.configFile = {
        "mpv/shaders/anime4k".source = pkgs.fetchFromGitHub {
            owner = "bloc97";
            repo = "Anime4K";
            rev = "8e39551ce96ed172605c89b7dd8be855b5502cc9";
            hash = "sha256-01js/vr+kpFAm2Hfj64ad+odZKYiC9TZiTTj6mwAFd8=";
        } + "/glsl";
        "mpv/shaders/aiupscale".source = pkgs.fetchFromGitHub {
            owner = "Alexkral";
            repo = "AviSynthAiUpscale";
            rev = "d04cf8154e4ba9914f3ead0dec9f7a4a7df7369f";
            hash = "sha256-MKW7y/I+LDAsOU3ROVN/ySrTYJ0YMN+P/oBB7x6HXys=";
        } + "/mpv user shaders";
    };
}
