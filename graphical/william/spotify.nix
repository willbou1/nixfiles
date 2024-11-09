{
  config,
  inputs,
  pkgs,
  ...
}: let
  spicePkgs = inputs.spicetify-nix.legacyPackages.${pkgs.system};
in
  with config.lib.stylix.colors; {
    home.persistence."/persist/home/william".directories = [
      ".config/spotify"
    ];
    programs.spicetify = {
      enable = true;

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

      colorScheme = "custom";
      customColorScheme = {
        text = base07;
        subtext = base05;
        diebar-text = base07;
        main = base00;
        sidebar = base00;
        player = base00;
        card = base00;
        shadow = base00;
        selected-row = base05;
        button = base08;
        button-active = base0F;
        button-disabled = base0A;
        tab-active = base09;
        notification = base0C;
        notification-error = base09;
        misc = base09;
      };
    };
  }
