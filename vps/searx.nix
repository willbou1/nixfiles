{
  lib,
  pkgs,
  config,
  ...
}:
with builtins; let
  hostName = config.networking.hostName;
  suffix = config.networking.suffix;
  searxDomain = "search.${hostName}.${suffix}";
  morty = config.services.morty;
in {
  sops = {
    secrets = {
      "searx/secret_key".owner = "uwsgi";
    };
    templates = {
      "searx_secrets".content = ''
        SEARXNG_SECRET="${config.sops.placeholder."searx/secret_key"}"
      '';
    };
  };

  security.acme.certs."${hostName}.${suffix}".extraDomainNames = [searxDomain];
  services.nginx.virtualHosts = {
    ${searxDomain} = {
      useACMEHost = "${hostName}.${suffix}";
      acmeRoot = "/var/lib/acme/challenge-${hostName}-${suffix}";
      forceSSL = true;
      locations = {
        "/" = {
          extraConfig = ''
            uwsgi_pass unix:${config.services.searx.uwsgiConfig.socket};
          '';
        };
        "/static/" = {
          alias = "${config.services.searx.package}/share/static/";
        };
        "/morty/" = {
          proxyPass = "http://${morty.listenAddress}:${toString morty.port}/";
          extraConfig = ''
            proxy_set_header   Host             $host;
            proxy_set_header   Connection       $http_connection;
            proxy_set_header   X-Real-IP        $remote_addr;
            proxy_set_header   X-Forwarded-For  $proxy_add_x_forwarded_for;
            proxy_set_header   X-Scheme         $scheme;
          '';
        };
      };
    };
  };

  # TODO change this if multiple uwsgi vassals in the future
  systemd.services.uwsgi.serviceConfig.EnvironmentFile = assert (length (attrNames config.services.uwsgi.instance.vassals)) <= 1;
    config.sops.templates."searx_secrets".path;
  users.groups.searx.members = ["nginx"];
  services = {
    searx = {
      enable = true;
      runInUwsgi = true;
      uwsgiConfig = {
        disable-logging = false;
        http = "";
        socket = "/run/searx/searx.sock";
        chmod-socket = "660";
      };
      limiterSettings = {
        real_ip = {
          x_for = 1;
          ipv4_prefix = 32;
          ipv6_prefix = 56;
        };
        botdetection = {
          ip_limit = {
            filter_link_local = true;
            link_token = true;
          };
        };
      };
      settings = {
        enable_plugins = [
          "Basic Calculator"
          "Hash plugin"
          "Tor check plugin"
          "Opean Access DOI rewrite"
          "Hostnames plugin"
          "Unit converter plugin"
          "Tracker URL remover"
        ];
        general = {
          debug = false;
          instance_name = "searx";
        };
        ui = {
          infinite_scroll = true;
          default_locale = "en";
          search_on_category_select = true;
          hotkeys = "vim";
          center_alignment = true;
        };
        engines = [
          {
            name = "brave";
            disabled = true;
          }
          {
            name = "yahoo";
            disabled = false;
          }
          {
            name = "erowid";
            disabled = true;
          }
          {
            name = "naver";
            disabled = false;
          }
        ];
        server = {
          #image_proxy = true;
          base_url = "https://${searxDomain}";
          public_instance = false;
          limiter = true;
        };
        #result_proxy = {
        #    url = "http://${morty.listenAddress}:${toString morty.port}/";
        #};
      };
    };
    #morty = {
    #    enable = true;
    #    listenAddress = "127.0.0.1";
    #    port = 3000;
    #};
  };
}
