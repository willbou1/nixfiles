{config, ...}: {
  xdg = {
    enable = true;
    mimeApps.enable = true;
    userDirs = let
      home = config.home.homeDirectory;
    in {
      enable = true;
      music = "${home}/priv/music";
      pictures = "${home}/priv/pictures";
      videos = "${home}/priv/videos";
      download = "${home}/priv/downloads";
      documents = "${home}/priv/documents";
      desktop = "${home}/priv/desktop";
      templates = "${home}/priv/templates";
      publicShare = "${home}/priv/public_share";
    };
  };
}
