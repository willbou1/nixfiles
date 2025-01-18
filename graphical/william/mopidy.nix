{
  pkgs,
  config,
  ...
}: rec {
  sops = {
    secrets = {
      "spotify/config" = {};
    };
  };
  services.mopidy = {
    enable = true;
    extensionPackages = with pkgs; [
      mopidy-spotify
      mopidy-mpris
      mopidy-local
      mopidy-soundcloud
      mopidy-jellyfin
      mopidy-bandcamp
      mopidy-mpd
    ];
    extraConfigFiles = [config.sops.secrets."spotify/config".path];
    settings = {
      file = {
        media_dirs = ["~/priv/music"];
        follow_symlinks = true;
      };
      audio.output = "alsasink";
    };
  };
  programs.ncmpcpp = {
    enable = true;
    mpdMusicDir = builtins.head services.mopidy.settings.file.media_dirs;
    bindings = [
      {
        key = "j";
        command = "scroll_down";
      }
      {
        key = "k";
        command = "scroll_up";
      }
      {
        key = "J";
        command = ["select_item" "scroll_down"];
      }
      {
        key = "K";
        command = ["select_item" "scroll_up"];
      }
    ];
  };
}
