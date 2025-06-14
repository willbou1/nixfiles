{config, ...}: let
  swaylock = config.programs.swaylock.package;
in {
  services.swayidle = {
    timeouts = [
      {
        timeout = 600;
        command = "${swaylock}/bin/swaylock -f --grace 0";
      }
    ];
    events = [
      {
        event = "before-sleep";
        command = "${swaylock}/bin/swaylock -f --grace 0";
      }
    ];
  };
}
