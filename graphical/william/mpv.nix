{pkgs, ...}: let
  mpv-unwrapped = pkgs.mpv-unwrapped.override {
    vapoursynthSupport = true;
    ffmpeg = pkgs.ffmpeg_6-full;
  };
  mpv = pkgs.mpv-unwrapped.wrapper {
    mpv = mpv-unwrapped;
    extraMakeWrapperArgs = [
      "--prefix"
      "LD_LIBRARY_PATH"
      ":"
      "/run/opengl-driver/lib:${pkgs.vapoursynth-mvtools}/lib/vapoursynth"
      # make mpv work with nvidia-offload automagically
      "--set"
      "__NV_PRIME_RENDER_OFFLOAD"
      "1"
      "--set"
      "__NV_PRIME_RENDER_OFFLOAD_PROVIDER"
      "NVIDIA-G0"
      "--set"
      "__GLX_VENDOR_LIBRARY_NAME"
      "nvidia"
      "--set"
      "__VK_LAYER_NV_optimus"
      "NVIDIA_only"
    ];
    scripts = [
      pkgs.mpvScripts.uosc
      pkgs.mpvScripts.mpris
      pkgs.mpvScripts.thumbfast
      pkgs.mpvScripts.sponsorblock
      pkgs.mpvScripts.webtorrent-mpv-hook
    ];
  };
  svpWrapper = pkgs.writeShellScriptBin "svp" ''
    regex='(https?|ftp|file)://[-[:alnum:]\+&@#/%?=~_|!:,.;]*[-[:alnum:]\+&@#/%=~_|]'
    mpvfile='/tmp/mpvfile.webm'
    if [[ $1 =~ $regex ]]
    then
        rm -f $mpvfile
        mkfifo $mpvfile
        ${pkgs.yt-dlp}/bin/yt-dlp -o - $1 > $mpvfile &
        SVPManager $mpvfile
    else
        SVPManager $1
    fi
  '';
in {
  home = {
    packages = [
      # TODO check if SVP is fixed
      (pkgs.svp.override {
        customMpv = svpWrapper;
      })
      svpWrapper
    ];
    persistence."/persist/home/william".directories = [
      ".local/state/mpv"
      ".local/share/SVP4"
    ];
  };
  programs.mpv = {
    enable = true;
    package = mpv;
    config = {
      slang = "kor,ko,eng,en,enUS";
      sid = "auto";
      sub-visibility = false;
      sub-margin-y = 50;
      sub-back-color = "0.0/0.0/0.0/0.45";
      sub-border-style = "background-box";
      sub-border-size = 0;
      demuxer-mkv-subtitle-preroll = true;
      subs-with-matching-audio = false;

      osc = false;
      osd-bar = false;
      border = false;
      #video-sync = "display-resample"; # TODO causes freezes I think on haskellslayslay

      alang = "jpn,jp,kor,ko,eng,en";
      af = "acompressor=ratio=4,loudnorm";

      window-scale = 1;
      load-osd-console = true;
      osd-duration = 2000;

      hwdec = "auto-copy";
      profile = "gpu-hq";
      hwdec-codecs = "all";
      dscale = "mitchell";
      cscale = "spline64";
      input-ipc-server = "/tmp/mpvsocket";
      hr-seek-framedrop = false;

      keep-open = true;
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
      "ALT+s" = "cycle secondary-sid";
      "ALT+w" = "cycle_values panscan 0 1";
      "ALT+k" = "add sub-scale +0.1";
      "ALT+j" = "add sub-scale -0.1";
      "ALT+v" = "vf toggle vflip";
      "ALT+h" = "vf toggle hflip";
      "M" = "af toggle lavfi=[pan=mono|c0=c1]";
      "ALT+r" = "cycle_values video-rotate 90 180 270 0";
      "CTRL+v" = "cycle secondary-sub-visibility";
      "CTRL+c" = "no-osd change-list glsl-shaders set \"\"";
      "CTRL+5" = "no-osd change-list glsl-shaders set \"~~/shaders/anime4k/Restore/Anime4K_Clamp_Highlights.glsl:~~/shaders/anime4k/Restore/Anime4K_Restore_CNN_VL.glsl:~~/shaders/anime4k/Upscale/Anime4K_Upscale_CNN_x2_VL.glsl:~~/shaders/anime4k/Upscale/Anime4K_AutoDownscalePre_x2.glsl:~~/shaders/anime4k/Upscale/Anime4K_AutoDownscalePre_x4.glsl:~~/shaders/anime4k/Upscale/Anime4K_Upscale_CNN_x2_M.glsl\"; show-text \"Anime4K: Mode A (HQ)\"";
      "CTRL+6" = "no-osd change-list glsl-shaders set \"~~/shaders/anime4k/Restore/Anime4K_Clamp_Highlights.glsl:~~/shaders/anime4k/Restore/Anime4K_Restore_CNN_Soft_VL.glsl:~~/shaders/anime4k/Upscale/Anime4K_Upscale_CNN_x2_VL.glsl:~~/shaders/anime4k/Upscale/Anime4K_AutoDownscalePre_x2.glsl:~~/shaders/anime4k/Upscale/Anime4K_AutoDownscalePre_x4.glsl:~~/shaders/anime4k/Upscale/Anime4K_Upscale_CNN_x2_M.glsl\"; show-text \"Anime4K: Mode B (HQ)\"";
      "CTRL+7" = "no-osd change-list glsl-shaders set \"~~/shaders/anime4k/Restore/Anime4K_Clamp_Highlights.glsl:~~/shaders/anime4k/Upscale/Anime4K_Upscale_Denoise_CNN_x2_VL.glsl:~~/shaders/anime4k/Upscale/Anime4K_AutoDownscalePre_x2.glsl:~~/shaders/anime4k/Upscale/Anime4K_AutoDownscalePre_x4.glsl:~~/shaders/anime4k/Upscale/Anime4K_Upscale_CNN_x2_M.glsl\"; show-text \"Anime4K: Mode C (HQ)\"";
      "CTRL+8" = "no-osd change-list glsl-shaders set \"~~/shaders/anime4k/Restore/Anime4K_Clamp_Highlights.glsl:~~/shaders/anime4k/Restore/Anime4K_Restore_CNN_VL.glsl:~~/shaders/anime4k/Upscale/Anime4K_Upscale_CNN_x2_VL.glsl:~~/shaders/anime4k/Restore/Anime4K_Restore_CNN_M.glsl:~~/shaders/anime4k/Upscale/Anime4K_AutoDownscalePre_x2.glsl:~~/shaders/anime4k/Upscale/Anime4K_AutoDownscalePre_x4.glsl:~~/shaders/anime4k/Upscale/Anime4K_Upscale_CNN_x2_M.glsl\"; show-text \"Anime4K: Mode A+A (HQ)\"";
      "CTRL+9" = "no-osd change-list glsl-shaders set \"~~/shaders/anime4k/Restore/Anime4K_Clamp_Highlights.glsl:~~/shaders/anime4k/Restore/Anime4K_Restore_CNN_Soft_VL.glsl:~~/shaders/anime4k/Upscale/Anime4K_Upscale_CNN_x2_VL.glsl:~~/shaders/anime4k/Upscale/Anime4K_AutoDownscalePre_x2.glsl:~~/shaders/anime4k/Upscale/Anime4K_AutoDownscalePre_x4.glsl:~~/shaders/anime4k/Restore/Anime4K_Restore_CNN_Soft_M.glsl:~~/shaders/anime4k/Upscale/Anime4K_Upscale_CNN_x2_M.gls\"; show-text \"Anime4K: Mode B+B (HQ)\"";
      "CTRL+0" = "no-osd change-list glsl-shaders set \"~~/shaders/anime4k/Restore/Anime4K_Clamp_Highlights.glsl:~~/shaders/anime4k/Upscale/Anime4K_Upscale_Denoise_CNN_x2_VL.glsl:~~/shaders/anime4k/Upscale/Anime4K_AutoDownscalePre_x2.glsl:~~/shaders/anime4k/Upscale/Anime4K_AutoDownscalePre_x4.glsl:~~/shaders/anime4k/Restore/Anime4K_Restore_CNN_M.glsl:~~/shaders/anime4k/Upscale/Anime4K_Upscale_CNN_x2_M.glsl\"; show-text \"Anime4K: Mode C+A (HQ)\"";
    };
    scriptOpts = {
      uosc = {
        scale = 1.3;
        ui_scale = 1.3;
        scale_fullscreen = 1.3;
        autoload = true;
        top_bar = "always";
        top_bar_controls = "no";
      };
      thumbfast = {
        max_height = 450;
        max_width = 450;
        hwdec = true;
        network = true;
      };
    };
  };

  xdg.configFile = {
    "mpv/shaders/anime4k".source =
      pkgs.fetchFromGitHub {
        owner = "bloc97";
        repo = "Anime4K";
        rev = "8e39551ce96ed172605c89b7dd8be855b5502cc9";
        hash = "sha256-01js/vr+kpFAm2Hfj64ad+odZKYiC9TZiTTj6mwAFd8=";
      }
      + "/glsl";
    "mpv/shaders/ai_upscale".source =
      pkgs.fetchFromGitHub {
        owner = "Alexkral";
        repo = "AviSynthAiUpscale";
        rev = "d04cf8154e4ba9914f3ead0dec9f7a4a7df7369f";
        hash = "sha256-MKW7y/I+LDAsOU3ROVN/ySrTYJ0YMN+P/oBB7x6HXys=";
      }
      + "/mpv user shaders";
  };
}
