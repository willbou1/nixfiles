{
  # Make some packages inside nixpkgs point to the nixpkgs-unstable version.
  # TODO Keep an eye on these packages closely in case something breaks.
  bleedingEdgePackages = [
    # kernels
    #"linuxPackages_zen"
    #"linuxPackages_latest"

    # video
    #"mpv"
    #"mpv-unwrapped"

    # cli
    "kitty"
    "fastfetch"

    # apps
    "element-desktop"
    "libreoffice-fresh"
    "steam"
    "bitwarden"
    "modrinth-app"
    "firefox"
    "kicad"
    "gimp"

    # misc
    "OVMF"
    "virtiofsd"
  ];

  # Nixpkgs-unstable will get patched with these PRs. The patched version can
  # be accessed as unstable in system and home-manager modules. Note that
  # inputs.unstable points to the original nixpkgs-unstable.
  unstableNixpkgsPRs = [
#    {
#      name = "Mautrix-discord module";
#      id = 355025;
#      sha256 = "1skhw6w4mnc4n9paha24kg1paqhl8837jmj86yvgyd9bd479qjrf";
#    }
  ];
}
