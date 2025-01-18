{
  config,
  pkgs,
  lib,
  ...
}: let
  qutebrowser = pkgs.qutebrowser.override {
    enableWideVine = true;
  };
  qutebrowser-setup = pkgs.writeShellScript "qutebrowser-setup" (
    builtins.concatStringsSep "\n" (builtins.map (n: "${config.programs.qutebrowser.package}/share/qutebrowser/scripts/dictcli.py install ${n}") config.programs.qutebrowser.settings.spellcheck.languages)
  );
in {
  imports = lib.mine.autoInclude ./. [];
  xdg.mimeApps.defaultApplications = {
    "text/html" = "org.qutebrowser.qutebrowser.desktop";
    "x-scheme-handler/http" = "org.qutebrowser.qutebrowser.desktop";
    "x-scheme-handler/https" = "org.qutebrowser.qutebrowser.desktop";
    "x-scheme-handler/about" = "org.qutebrowser.qutebrowser.desktop";
    "x-scheme-handler/unknown" = "org.qutebrowser.qutebrowser.desktop";
  };
  home.persistence."/persist/home/william" = {
    directories = [
      ".local/share/qutebrowser"
    ];
    files = [
      ".config/qutebrowser/quickmarks"
    ];
  };

  systemd.user.services.qutebrowser-setup = {
    Unit = {
      Description = "Fetch qutebrowser dicts for my languages";
      After = ["network-online.target"];
      Wants = ["network-online.target"];
    };
    Service = {
      Type = "oneshot";
      ExecStart = qutebrowser-setup;
    };
    Install = {
      WantedBy = ["default.target"];
    };
  };

  programs.qutebrowser = {
    enable = true;
    package = qutebrowser;
    domainSettings = {
      "https://teams.microsoft.com" = {
        content = {
          notifications.enabled = true;
          javascript.clipboard = "access-paste";
        };
        colors.webpage.darkmode.enabled = false;
      };
      "https://www.netflix.com".content.notifications.enabled = false;
      "https://www.facebook.com".content.notifications.enabled = false;
      "https://mail.google.com" = {
        content.register_protocol_handler = true;
        colors.webpage.darkmode.enabled = false;
      };
      "https://github.com".content.javascript.clipboard = "access-paste";
    };
    settings = {
      # Enable additional hardware acceleration
      qt.args = [
        "ignore-gpu-blocklist"
        "enable-gpu-rasterization"
        "enable-accelerated-2d-canvas"
      ];

      window.transparent = true;
      editor.command = ["${config.home.terminal}" "nvim" "{file}" "-c" "normal {line}G{column0}l"];
      zoom.default = "135%";
      content = {
        pdfjs = true;
        autoplay = false;
        fullscreen.overlay_timeout = 0;
        blocking = {
          method = "both";
          adblock.lists = [
            "https://easylist.to/easylist/easylist.txt"
            "https://easylist.to/easylist/easyprivacy.txt"
            "https://easylist-downloads.adblockplus.org/easylistdutch.txt"
            "https://easylist-downloads.adblockplus.org/abp-filters-anti-cv.txt"
            "https://www.i-dont-care-about-cookies.eu/abp/"
            "https://secure.fanboy.co.nz/fanboy-cookiemonster.txt"
          ];
        };
        #user_stylesheets = [];
      };
      input.insert_mode = {
        auto_load = true;
        plugins = true;
      };
      url = {
        default_page = "https://startpage.com";
        start_pages = ["https://startpage.com"];
        open_base_url = true;
      };
      hints = {
        radius = 10;
        uppercase = true;
      };
      scrolling.smooth = true;
      auto_save = {
        session = true;
        interval = 5000;
      };
      colors.webpage.darkmode.enabled = true;
      spellcheck.languages = ["en-US" "fr-FR"];
      statusbar = {
        show = "always";
        position = "top";
        widgets = [
          "keypress"
          "search_match"
          "url"
          "history"
          "tabs"
          "progress"
        ];
      };
      tabs = {
        show = "multiple";
        position = "bottom";
        select_on_remove = "last-used";
        mode_on_change = "restore";
        title.format = "{audio} {current_title}";
      };
      downloads = {
        position = "bottom";
        location = {
          directory = config.xdg.userDirs.download;
          prompt = true;
          suggestion = "filename";
        };
        remove_finished = 20;
      };
    };
    searchEngines = {
      #"DEFAULT" = "https://search.ourmiraculous.com/searx/search?q={}";
      #"DEFAULT" = "https://startpage.com/do/search?q={}";
      "DEFAULT" = "https://duckduckgo.com/?q={}";
      "!s" = "https://startpage.com/do/search?q={}";
      "!w" = "https://en.wikipedia.org/wiki/Special:Search?search={}&amp;go=Go&amp;ns0=1";
      "!g" = "https://www.google.ca/search?q={}";
      "!d" = "https://duckduckgo.com/?q={}";
      "!i" = "https://inv.nadeko.net/search?q={}";
      "!p" = "https://piped.video/results?search_query={}";
      "!y" = "http://www.youtube.com/results?search_query={}&page={{startPage?}}&utm_source=opensearch";
      "!m" = "https://mydramalist.com/search?q={}";
      "!nf" = "https://netflix.com/search?q={}";
      "!a" = "https://www.amazon.ca/s?k={}";
      "!ne" = "https://www.newegg.ca/p/pl?d={}";
      "!sc" = "https://soundcloud.com/search?q={}";
      "!gm" = "https://mail.google.com/mail/u/0/#search/{}";

      "rd" = "https://reddit.com/r/{}";
    };
    keyBindings = {
      normal = {
        ",c" = "open https://chat.openai.com";
        ",C" = "open -t https://chat.openai.com";
        ",n" = "open https://mynixos.com";
        ",N" = "open -t https://mynixos.com";
        ",d" = "open https://noogle.dev";
        ",D" = "open -t https://noogle.dev";
        ",g" = "open https://github.com";
        ",G" = "open -t https://github.com";
        ",t" = "config-cycle colors.webpage.darkmode.enabled";
      };
    };
    extraConfig = ''
      c.tabs.padding = {'top':5,'bottom':5,'left':10,'right':10};
      config.unbind('<Ctrl-a>', mode='normal');
    '';
  };
}
