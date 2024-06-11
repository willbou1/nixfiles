{ config, pkgs, lib, ...}: let
    qutebrowser = pkgs.qutebrowser.override {
        enableWideVine = true;
    };
    qutebrowser-setup = pkgs.writeShellScript "qutebrowser-setup" (
        builtins.concatStringsSep "\n" (builtins.map (n: "${config.programs.qutebrowser.package}/share/qutebrowser/scripts/dictcli.py install ${n}") config.programs.qutebrowser.settings.spellcheck.languages)
    );
in {
    imports = [
        ./theme.nix
        ./bitwarden.nix
    ];
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
            After = [ "network-online.target" ];
            Wants = [ "network-online.target" ];
        };
        Service = {
            Type = "oneshot";
            ExecStart = qutebrowser-setup;
        };
        Install = {
            WantedBy = [ "default.target" ];
        };
    };

    programs.qutebrowser = {
        enable = true;
        package = qutebrowser;
        settings = {
            window.transparent = true;
            editor.command = ["${config.home.terminal}" "nvim" "{file}" "-c" "normal {line}G{column0}l"];
            zoom.default = "135%";
            content = {
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
                user_stylesheets = [
                    (config.xdg.configHome +
                    "/qutebrowser/stylesheets/youtube.css")
                ];
            };
            input.insert_mode.auto_load = true;
            url = {
                default_page = "https://search.ourmiraculous.com";
                start_pages = [ "https://search.ourmiraculous.com" ];
                open_base_url = true;
            };
            hints = {
                radius = 10;
                uppercase = true;
            };
            scrolling.smooth = true;
            auto_save.session = true;
            colors.webpage.darkmode.enabled = true;
            spellcheck.languages = [ "en-US" "fr-FR" ];
            statusbar = {
                show = "always";
                position = "top";
            };
            tabs = {
                show = "multiple";
                position = "bottom";
                select_on_remove = "last-used";
                title.format = "{audio} {current_title}";
            };
            downloads = {
                position = "bottom";
                location = {
                    directory = config.xdg.userDirs.download;
                    prompt = false;
                    suggestion = "filename";
                };
                remove_finished = 15;
            };
        };
        searchEngines = {
            #"DEFAULT" = "https://search.ourmiraculous.com/searx/search?q={}";
            "DEFAULT" = "https://duckduckgo.com/?q={}";
            "!w" = "https://en.wikipedia.org/wiki/Special:Search?search={}&amp;go=Go&amp;ns0=1";
            "!g" = "https://www.google.ca/search?q={}";
            "!d" = "https://duckduckgo.com/?q={}";
            "!i" = "https://invidious.asir.dev/search?q={}";
            "!p" = "https://piped.video/results?search_query={}";
            "!y" = "http://www.youtube.com/results?search_query={}&page={{startPage?}}&utm_source=opensearch";
            "!m" = "https://mydramalist.com/search?q={}";
            "!nf" = "https://netflix.com/search?q={}";
            "!a" = "https://www.amazon.ca/s?k={}";
            "!ne" = "https://www.newegg.ca/p/pl?d={}";
            "!s" = "https://soundcloud.com/search?q={}";

            "rd" = "https://reddit.com/r/{}";
        };
        extraConfig = ''
            c.tabs.padding = {'top':5,'bottom':5,'left':10,'right':10};
        '';
    };

    # TODO: Remember to keep this up to date
    home.file.".local/share/qutebrowser/greasemonkey/youtube-adblock.user.js".text = ''
// ==UserScript==
// @name Skip YouTube ads
// @description Skips the ads in YouTube videos
// @run-at document-start
// @include *.youtube.com/*
// ==/UserScript==

document.addEventListener('load', () => {
    const btn = document.querySelector('.videoAdUiSkipButton,.ytp-ad-skip-button-modern')
    if (btn) {
        btn.click()
    }
    const ad = [...document.querySelectorAll('.ad-showing')][0];
    if (ad) {
        document.querySelector('video').currentTime = 9999999999;
    }
}, true);
    '';
    xdg.configFile."qutebrowser/stylesheets/youtube.css".text = ''
.ytp-gradient-bottom{
   display:none !important
}
    '';
}
