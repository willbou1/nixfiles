{
  lib,
  config,
  pkgs,
  inputs,
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
  home.persistence."/persist/home/william" = {
    directories = [
      ".gnupg"
      ".ssh"
    ];
    allowOther = true;
  };

  home.packages = with pkgs; [
    home-manager
  ];

  home.stateVersion = "24.05";
}
