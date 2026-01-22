{
  config,
  pkgs,
  ...
}: {
  boot = {
    kernelModules = ["evdi"];
  };
  environment.systemPackages = with pkgs; [
    displaylink
  ];
}
