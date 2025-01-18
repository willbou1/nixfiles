{
  # Make some packages inside nixpkgs point to the nixpkgs-unstable version.
  # TODO Keep an eye on these packages closely in case something breaks.
  bleedingEdgePackages = [
    # kernels
    "linuxPackages_zen"
    "linuxPackages_latest"

    # video
    "mpv"
    "mpv-unwrapped"
    "svp"

    # cli
    "kitty"
    "neovim"
    "neovim-unwrapped"
    "fastfetch"

    # apps
    "element-desktop"
    "libreoffice-fresh"
    "steam"
    "bitwarden"
    "modrinth-app"
    "firefox"
    "gimp"

    # vps
    "mautrix-meta" # CVE with libolm

    # misc
    "OVMF"
  ];

  # Nixpkgs-unstable will get patched with these PRs. The patched version can
  # be accessed as unstable in system and home-manager modules. Note that
  # inputs.unstable points to the original nixpkgs-unstable.
  unstableNixpkgsPRs = [
    {
      name = "Mautrix-discord module";
      id = 355025;
      sha256 = "0lmmcbw1mpjngbdpm9q25dbbbf70i8n8z7yk1pns61rf8mryyk20";
    }
  ];
}
