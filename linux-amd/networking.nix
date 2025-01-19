{pkgs, ...}: {
  networking = {
    hostName = "linux-amd";
    ip = "10.0.0.160";
    subnetLength = 24;
    subnet = "10.0.0.0";
    gateway = "10.0.0.1";
    mainInterface = "wlp6s0";
  };
  environment = {
    persistence."/persist" = {
      directories = [
        "/var/lib/jellyfin"
        "/var/cache/jellyfin"
      ];
    };
    systemPackages = with pkgs; [
      jellyfin-ffmpeg
    ];
  };
  networking.firewall = {
    allowedTCPPorts = [21];
    allowedTCPPortRanges = [
      {
        from = 2000;
        to = 2030;
      }
    ];
  };
  services = {
    deluge = {
      web = {
        enable = true;
        openFirewall = true;
      };
      config.download_location = "/data/torrents";
    };
    jellyfin = {
      enable = true;
      openFirewall = true;
    };
    vsftpd = {
      enable = true;
      localUsers = true;
      writeEnable = true;
      chrootlocalUser = true;
      allowWriteableChroot = true;
      forceLocalDataSSL = true;
      forceLocalLoginsSSL = true;
      rsaCertFile = ../resources/vsftpd.pem;
      localRoot = "/data";
      userlistEnable = true;
      userlist = [
        "william"
      ];
      extraConfig = ''
        ssl_ciphers=HIGH
        require_ssl_reuse=YES
        pasv_enable=Yes
        pasv_max_port=2030
        pasv_min_port=2000
      '';
    };
  };
}
