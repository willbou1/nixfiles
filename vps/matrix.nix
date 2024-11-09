{
  config,
  pkgs,
  ...
}: let
  hostName = config.networking.hostName;
  suffix = config.networking.suffix;
  elementAddress = "element.${hostName}.${suffix}";
  matrixAddress = "${hostName}.${suffix}";
  synapseAddress = "synapse.${hostName}.${suffix}";
  element-web-custom = pkgs.element-web.override {
    conf = {
      default_server_config = {
        "m.homeserver" = {
          base_url = "https://${matrixAddress}";
          server_name = matrixAddress;
        };
        "m.identity_server" = {
          base_url = "https://vector.im";
        };
      };
      disable_custom_urls = true;
      disable_guests = true;
      show_labs_settings = true;
      default_theme = "dark";
    };
  };
in {
  sops = {
    secrets."synapse/manhole" = {};
    secrets."synapse/registration" = {};
    secrets."synapse/double_puppet" = {};
    templates."double_puppet.yaml" = {
      owner = "matrix-synapse";
      content = ''
        id: duoblepuppet
        url:
        as_token: "${config.sops.placeholder."synapse/double_puppet"}"
        hs_token: "shouldnotmatter"
        sender_localpart: "shouldnotmatter"
        rate_limited: false
        namespaces:
            users:
            - regex: '@.*:${hostName}\.${suffix}$'
              exclusive: false
      '';
    };
    templates."synapse_secrets" = {
      owner = "matrix-synapse";
      content = ''
        database:
            name: psycopg2
            args:
                user: synapse
                password: "${config.sops.placeholder."postgresql/synapse"}"
                dbname: synapse
                host: localhost
                port: 5432
                cp_min: 5
                cp_min: 10
        manhole_settings:
            username: mrnobody
            password: "${config.sops.placeholder."synapse/manhole"}"
        registration_shared_secret: "${config.sops.placeholder."synapse/registration"}"
        turn_shared_secret: "${config.sops.placeholder."coturn/secret"}"
      '';
    };
  };

  security.acme.certs."${hostName}.${suffix}".extraDomainNames = [
    elementAddress
    synapseAddress
    matrixAddress
  ];
  services.nginx.virtualHosts = {
    "${elementAddress}" = {
      useACMEHost = "${hostName}.${suffix}";
      acmeRoot = "/var/lib/acme/challenge-${hostName}-${suffix}";
      forceSSL = true;
      locations."/" = {
        root = "${element-web-custom}";
        index = "index.html";
        extraConfig = ''
          add_header X-Frame-Options SAMEORIGIN;
          add_header X-Content-Type-Options nosniff;
          add_header X-XSS-Protection "1; mode=block";
          add_header Content-Security-Policy "frame-ancestors 'none'";
        '';
      };
    };
    "${synapseAddress}" = {
      useACMEHost = "${hostName}.${suffix}";
      acmeRoot = "/var/lib/acme/challenge-${hostName}-${suffix}";
      forceSSL = true;
      locations."/" = {
        root = "${pkgs.synapse-admin}";
        extraConfig = ''
          add_header X-Frame-Options SAMEORIGIN;
          add_header X-Content-Type-Options nosniff;
          add_header X-XSS-Protection "1; mode=block";
          add_header Content-Security-Policy "frame-ancestors 'none'";
        '';
      };
    };
    "${matrixAddress}" = {
      useACMEHost = "${hostName}.${suffix}";
      acmeRoot = "/var/lib/acme/challenge-${hostName}-${suffix}";
      forceSSL = true;
      locations = {
        "~* ^(\/_matrix|\/_synapse\/client)" = {
          proxyPass = "http://localhost:8008";
          extraConfig = ''
            proxy_set_header X-Forwarded-For $remote_addr;
            proxy_set_header X-Forwarded-Proto $scheme;
            proxy_set_header Host $host;
            client_max_body_size 1024M;
          '';
        };
        # Expose admin API (good password needs to be in place)
        "~* ^(\/_synapse\/admin)" = {
          proxyPass = "http://localhost:8008";
          extraConfig = ''
            proxy_set_header X-Forwarded-For $remote_addr;
            proxy_set_header X-Forwarded-Proto $scheme;
            proxy_set_header Host $host;
            client_max_body_size 1024M;
          '';
        };
        # Frontend proxy worker
        #"^/_matrix/client/(r0|v3|unstable)/keys/upload" = {
        #    proxyPass = "http://localhost:8008";
        #    extraConfig = ''
        #        proxy_set_header X-Forwarded-For $remote_addr;
        #        proxy_set_header X-Forwarded-Proto $scheme;
        #        proxy_set_header Host $host;
        #        client_max_body_size 1024M;
        #    '';
        #};
      };
    };
  };

  services.matrix-synapse = {
    enable = true;
    dataDir = "/srv/matrix-synapse";
    configureRedisLocally = true;
    extraConfigFiles = [
      config.sops.templates."synapse_secrets".path
    ];
    log = {
      disable_existing_loggers = true;
      formatters = {
        journal_fmt = {
          format = "%(name)s: [%(request)s] %(message)s";
        };
      };
      handlers = {
        journal = {
          class = "systemd.journal.JournalHandler";
          formatter = "journal_fmt";
        };
      };
      root = {
        handlers = [
          "journal"
        ];
        level = "CRITICAL";
      };
      version = 1;
    };
    settings = {
      url_preview_enabled = true;
      url_preview_in_range_blacklist = ["127.0.0.1" "::1/128"];
      suppress_key_server_warning = true;
      enable_registration = true;
      registration_requires_token = true;
      allow_guest_access = false;
      auto_join_rooms = ["#help:${matrixAddress}"];
      enable_metrics = true;
      server_name = matrixAddress;
      web_client_location = "https://${elementAddress}";
      app_service_config_files = [config.sops.templates."double_puppet.yaml".path];
    };
  };
}
