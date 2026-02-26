{
  lib,
  config,
  pkgs,
  ...
}:
with builtins;
with lib; let
  themeConfig = config.lib.stylix.colors {
    template = ./base16-theme.el.mustache;
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
  emacsWithPackages = (pkgs.emacsPackagesFor emacs).emacsWithPackages (epkgs:
    with epkgs; [
      # Custom sources
      (trivialBuild {
        pname = "combobulate";
        version = "unstable";

        src = pkgs.fetchFromGitHub {
          owner = "mickeynp";
          repo = "combobulate";
          rev = "38773810b5e532f25d11c6d1af02c3a8dffeacd7";
          hash = "sha256-gW0j0asfH4My+mBSt8pj3d51PigGWzFUhETydUI9xEg=";
        };
      })
      
      # Languages
      cmake-mode
      rust-mode
      haskell-mode
      csharp-mode
      lua-mode
      jupyter
      auctex
      nix-mode
      fish-mode

      # Editing
      undo-tree
      general
      evil
      evil-escape
      evil-surround
      evil-mc
      evil-tex
      evil-collection
      smartparens
      ace-window
      minimap

      # LSP and debugging
      lsp-mode
      lsp-ui
      lsp-haskell
      dap-mode
      flycheck

      # Completion
      helm
      helm-lsp
      helm-xref
      helm-c-yasnippet
      helm-tramp
      helm-projectile
      helm-descbinds
      corfu
      cape
      orderless

      yasnippet
      yasnippet-snippets
      projectile
      magit

      # Org
      toc-org
      org-appear
      org-modern
      org-fragtog

      (treesit-grammars.with-grammars (p: with p; [
        tree-sitter-css
        tree-sitter-html
        tree-sitter-javascript
        tree-sitter-json
        tree-sitter-typescript
        tree-sitter-tsx

        tree-sitter-c
        tree-sitter-cpp
        tree-sitter-glsl
        tree-sitter-rust
        tree-sitter-go
        tree-sitter-lua
        tree-sitter-python
        tree-sitter-nix

        tree-sitter-elisp
        
        tree-sitter-bash
        tree-sitter-fish

        tree-sitter-markdown
        tree-sitter-markdown-inline
        tree-sitter-yaml
        tree-sitter-toml
      ]))

      # Theme
      ligature
      nerd-icons
      rainbow-delimiters
      base16-theme
      highlight-defined
      highlight-numbers
      rainbow-identifiers
      diredfl
      smooth-scroll
      dashboard
      page-break-lines
      olivetti

      dired-subtree
      helpful
      restart-emacs
      default-text-scale

      # Libs
      dash
      shut-up
      f
      s

      # AI
      copilot
      gptel
    ]);
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
    package = emacsWithPackages;
    extraOptions = mkIf debug [
      "--debug-init"
    ];
    client.enable = true;
  };
  systemd.user.services.emacs = {
    Service = {
      WorkingDirectory = (toPath config.home.homeDirectory) + "/priv";
      Nice = -10;
    };
  };

  home = {
    persistence."/persist" = {
      directories = [
        ".config/emacs"
      ];
      # Tangled literate config
      files = map (n: ".config/doom/${n}.el") [
        "config"
        "playground"
        "custom"
      ];
    };
    packages = [emacsWithPackages];
  };
  xdg.configFile = {
    "emacs/init.el".text = with config.stylix; let
      percentageOpacity = floor (opacity.terminal * 100);
      fontSize = floor (fonts.sizes.terminal * 9);
      secrets = concatStringsSep "\n" (attrValues (mapAttrs (k: v: "(setq sops--${elemAt (split "/" k) 2} (f-read-text \"${config.sops.secrets.${k}.path}\"))") sops.secrets));
    in
      concatStringsSep "\n" [
        ''
          ;; -*- lexical-binding: t; -*-
          (require 'savehist)
          (add-to-list 'savehist-additional-variables '+theme-frame-alpha)
          (savehist-mode 1)

          (unless (boundp '+theme-frame-alpha)
           (setq +theme-frame-alpha ${toString percentageOpacity}))
          (require 'f)
        ''
        secrets
        (readFile ./init.el)
        ''
          (set-face-attribute 'default nil :height ${toString fontSize})
          (provide 'init)
          ;;;; init.el ends here
        ''
      ];
    "emacs/config".source = ./config;
    "emacs/splash".source = ./../../../resources/splash/emacs;
    "emacs/themes/base16-stylix-theme.el".source = themeConfig;
  };
}
