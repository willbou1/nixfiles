{
  inputs,
  lib,
  config,
  pkgs,
  ...
}: {
  sops = {
    secrets = {
      "deluge/localclient_password" = {};
      "deluge/william_password" = {};
    };
    templates."deluge-auth" = {
      content = ''
        localclient:${config.sops.placeholder."deluge/localclient_password"}:10
        william:${config.sops.placeholder."deluge/william_password"}:10
      '';
      owner = "deluge";
    };
  };
  environment = {
    persistence."/persist".directories = [
      "/var/lib/deluge"
    ];
    etc."deluge/deluge-auth".source = config.sops.templates."deluge-auth".path;
  };
  services.deluge = {
    enable = true;
    declarative = true;
    openFirewall = true;
    authFile = "/etc/deluge/deluge-auth";
    config = {
      allow_remote = true;
      incoming_interface = "tun0";
      outcoming_interface = "tun0";
      max_active_limit = 20;
      max_active_downloading = 15;
      max_active_seeding = 20;
      max_upload_speed = 80;
      move_completed = true;
      torrentfiles_location =
        config.services.deluge.config.download_location;
      move_completed_path =
        config.services.deluge.config.download_location + "/completed";
    };
  };
}
