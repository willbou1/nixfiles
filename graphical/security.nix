{
  lib,
  inputs,
  config,
  pkgs,
  ...
}: {
  security = {
    pam = {
      services.swaylock.unixAuth = true;
    };
  };
}
