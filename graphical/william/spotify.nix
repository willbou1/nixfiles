{
  inputs,
  pkgs,
  ...
}: let
  spicePkgs = inputs.spicetify-nix.legacyPackages.${pkgs.system};
in {
  home.persistence."/persist/home/william".directories = [
    ".config/spotify"
  ];
  programs.spicetify = {
    enable = true;

    # debug
    # spotifyPackage = pkgs.spotify.overrideAttrs (oldAttrs: {
    #   nativeBuildInputs = [pkgs.breakpointHook] ++ oldAttrs.nativeBuildInputs;
    # });

    # TODO remove this once the versions are compatible
    spicetifyPackage = pkgs.unstable.spicetify-cli;
    spotifyPackage = pkgs.spotify;

    enabledCustomApps = with spicePkgs.apps; [
      lyricsPlus
    ];

    enabledExtensions = with spicePkgs.extensions; [
      bookmark
      keyboardShortcut

      fullAppDisplay
      fullAlbumDate
      wikify
      history
      #genre
    ];
  };
}
