{pkgs, ...}: {
  environment.systemPackages = with pkgs; [
    alsa-tools # HDSPe tools for RME RayDAT
  ];
}
