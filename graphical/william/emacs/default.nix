{
  lib,
  config,
  pkgs,
  ...
}:
with builtins;
with lib; let
  themeConfig = config.lib.stylix.colors {
    template = ./doom/doom-base16-theme.el.mustache;
    extension = ".el";
  };
  useOverlay = true;
  debug = false;
  emacs =
    if useOverlay
    then
      pkgs.emacs-git-pgtk.override {
        withNativeCompilation = true;
      }
    else pkgs.emacs;
in rec {
  sops.secrets = {
    "emacs/ai-images-api-key" = {};
  };

  # TODO DEBUG find weird systemd environment bug
  #systemd.user.services.dummy-env-log = {
  #  Unit = {
  #    Description = "dummy";
  #  };
  #  Service = {
  #    Type = "oneshot";
  #    ExecStart = "${pkgs.bash}/bin/bash -c 'env | while read line; do echo $line; done'";
  #  };
  #  Install = {
  #    WantedBy = ["default.target"];
  #  };
  #};

  services.emacs = {
    enable = true;
    package = emacs;
    extraOptions = mkIf debug [
      "--debug-init"
    ];
    client.enable = true;
  };
  systemd.user.services.emacs = {
    #path = with pkgs; [
    #  clang
    #  gnumake
    #  cmake

    #];
    Service.WorkingDirectory = (toPath config.home.homeDirectory) + "/priv";
  };

  home = {
    persistence."/persist/home/william" = {
      directories = [
        ".config/emacs"
        ".local/share/doom"
      ];
      # Tangled literate config
      files = map (n: ".config/doom/${n}.el") [
        "config"
        "playground"
        "custom"
      ];
    };
    packages = [emacs];
  };
  xdg.configFile."doom/init.el".source = ./doom/init.el;
  xdg.configFile."doom/packages.el".source = ./doom/packages.el;
  xdg.configFile."doom/config.org".text = with config.stylix; let
    percentageOpacity = floor (opacity.terminal * 100);
    fontSize = floor (fonts.sizes.terminal * 1.22);
    bigFontSize = floor (fontSize * 1.5);
    secrets = concatStringsSep "\n" (attrValues (mapAttrs (k: v: "(setq sops--${elemAt (split "/" k) 2} (f-read-text \"${config.sops.secrets.${k}.path}\"))") sops.secrets));
  in
    concatStringsSep "\n" [
      ''
        #+BEGIN_SRC elisp :tangle yes
        (require 'f)
      ''
      secrets
      ''
        (setq doom-font (font-spec :family "${fonts.monospace.name}" :size ${toString fontSize} :weight 'semi-light)
              doom-variable-pitch-font (font-spec :family "${fonts.sansSerif.name}" :size ${toString (fontSize - 1)})
              doom-big-font (font-spec :family "${fonts.monospace.name}" :size ${toString bigFontSize}))

        (setq doom-theme 'doom-base16)
        (set-frame-parameter nil 'alpha-background ${toString percentageOpacity})
        (add-to-list 'default-frame-alist '(alpha-background . ${toString percentageOpacity}))
      ''
      "#+END_SRC\n"
      (readFile ./doom/config.org)
      ''
        #+BEGIN_SRC elisp :tangle yes
        (let ((playground-file (concat doom-private-dir "playground.el")))
              (if (file-exists-p playground-file)
                  (load playground-file)))
        #+END_SRC
      ''
    ];
  xdg.configFile."doom/themes/doom-base16-theme.el".source = themeConfig;
  xdg.configFile."doom/splash".source = ./../../../resources/splash/emacs;
}
