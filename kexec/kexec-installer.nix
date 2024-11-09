## USAGE
# $ nix-build kexec-installer.nix
# can be deployed remote like this
# $ rsync -aL -e ssh result/ root@host:
# $ ssh root@host ./kexec-installer
## Customize it like this
# # custom-installer.nix
# import ./kexec-installer.nix {
#   extraConfig = {pkgs, ... } {
#     user.extraUsers.root.openssh.authorizedKeys.keys = [ "<your-key>" ];
#     services.openssh = {
#        enable = true;
#        startWhenNeeded = true;
#     }
#   }
# }
# $ nix-build custom-installer.nix
# $ ls -la ./result
# TODO: make it fully automatic: https://gist.github.com/cleverca22/75e3db4dabeff05b743e9be77a2341b9#file-configuration-nix-L4-L19
{
  extraConfig ? {...}: {},
}:
let
  pkgs = import <nixpkgs> {};
  config = (import <nixpkgs/nixos> {
    configuration = {
      imports = [
        <nixpkgs/nixos/modules/installer/netboot/netboot-minimal.nix>
        extraConfig
      ];
    };
  }).config;
  inherit (config.system) build;
  kexecScript = pkgs.writeScript "kexec-installer" ''
    #!/bin/sh
    kexec --load ./bzImage \
      --initrd=./initrd.gz \
      --command-line "init=${config.system.build.toplevel}/init ${toString config.boot.kernelParams}"
    if systemctl --version >/dev/null 2>&1; then
      systemctl kexec
    else
      kexec -e
    fi
  '';
in pkgs.linkFarm "netboot" [
  { name = "initrd.gz"; path = "${build.netbootRamdisk}/initrd"; }
  { name = "bzImage";   path = "${build.kernel}/bzImage"; }
  { name = "kexec-installer"; path = kexecScript; }
]
