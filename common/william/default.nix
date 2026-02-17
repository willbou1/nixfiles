{
  lib,
  pkgs,
  ...
}:
with lib; {
  #new version
  imports = mine.autoInclude ./. [];

  home.username = "william";
  home.homeDirectory = "/home/william";
  sops = {
    defaultSopsFile = ./secrets.yaml;
    age.sshKeyPaths = ["/home/william/.ssh/id_ed25519"];
  };
  home.persistence."/persist" = {
    directories = [
      ".gnupg"
      ".ssh"
    ];
  };

  home.packages = with pkgs; [
    home-manager
  ];

  xdg.configFile."lftp/rc".text = ''
    set net:reconnect-interval-base 1
    set net:max-retries 1
  '';

  home.stateVersion = "25.11";
}
