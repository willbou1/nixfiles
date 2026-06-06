{
  pkgs,
  config,
  lib,
  ...
}:
with builtins; let
  # 1. Declarative, reproducible JSON default structure
  ytmConfig = toJSON {
    metadata.version = 1;
    general = {
      disableHardwareAcceleration = false;
      hideToTrayOnClose = false;
      showNotificationOnSongChange = false;
      startOnBoot = false;
      startMinimized = false;
    };
    appearance = {
      alwaysShowVolumeSlider = true;
      customCSSEnabled = true;
      customCSSPath = "/home/william/.config/YouTube Music Desktop App/stylix.css";
      zoom = "150";
      trayIconStyle = 0;
    };
    playback = {
      continueWhereYouLeftOff = true;
      continueWhereYouLeftOffPaused = true;
      enableSpeakerFill = false;
      progressInTaskbar = false;
      ratioVolume = false;
    };
    integrations = {
      companionServerEnabled = false;
      companionServerAuthTokens = null;
      companionServerCORSWildcardEnabled = false;
      discordPresenceEnabled = false;
      lastFMEnabled = false;
    };
    shortcuts = {
      playPause = "Space";
      next = "L";
      previous = "H";
      thumbsUp = "Shift+K";
      thumbsDown = "Shift+J";
      volumeUp = "K";
      volumeDown = "J";
    };
    state = {
      lastUrl = "https://music.youtube.com/playlist?list=OLAK5uy_lCOBCY9eZaLLYfS0sYNH0f-YRIPvGxOGI";
      lastPlaylistId = "OLAK5uy_lCOBCY9eZaLLYfS0sYNH0f-YRIPvGxOGI";
      lastVideoId = "X2x0w5kM2DY";
      windowBounds = null;
      windowMaximized = false;
    };
    lastfm = {
      api_key = "2a69bcf769a7a28a8bf2f6a5100accad";
      secret = "46eea23770a459a49eb4d26cbf46b41c";
      token = null;
      sessionKey = null;
      scrobblePercent = 50;
    };
    developer = {
      enableDevTools = false;
    };
  };

  defaultConfigJson = pkgs.writeText "ytm-config-default.json" ytmConfig;
  configDir = "${config.xdg.configHome}/YouTube Music Desktop App";
in {
  home = {
    persistence."/persist".directories = [
      ".config/YouTube Music Desktop App"
    ];

    packages = with pkgs; [
      unstable.ytmdesktop
    ];

    activation.ytmdesktop-writable-config = lib.hm.dag.entryAfter ["writeBoundary"] ''
      mkdir -p "${configDir}"
      
      if [ ! -f "${configDir}/config.json" ]; then
        cp "${defaultConfigJson}" "${configDir}/config.json"
        chmod +w "${configDir}/config.json"
      fi
    '';
  };

  xdg.configFile."YouTube Music Desktop App/stylix.css".text =
    with config.lib.stylix.colors.withHashtag; ''
      html:not(.style-scope) {
          --cust-ytmusic-dark-background: ${base00};
          --cust-ytmusic-dark-foreground: ${base01};

          --cust-ytmusic-light-text-1: ${base05};
          --cust-ytmusic-light-2: ${base06};
          --cust-ytmusic-light-3: ${base07};
          --cust-ytmusic-light-4: ${base04};
          --cust-ytmusic-light-5: ${base0D};

          --cust-ytmusic-medium-1: ${base03};
          --cust-ytmusic-medium-2: ${base02};
          --cust-ytmusic-medium-3: ${base0A};
          --cust-ytmusic-medium-4: ${base09};
          --cust-ytmusic-medium-5: ${base0B};

          --cust-ytmusic-subscribe-color: ${base0C};
          --cust-ytmusic-playbutton-color: ${base0D};
          --cust-ytmusic-tertiary-nav-button-color: ${base0E};
          --cust-ytmusic-nowplaying-color: ${base0F};
      }
    '' + readFile ./yt-music.css;
}
