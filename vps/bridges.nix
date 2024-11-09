{
  lib,
  config,
  ...
}:
with lib; let
  hostName = config.networking.hostName;
  suffix = config.networking.suffix;
  appserviceDiscordDir = "/var/lib/matrix-appservice-discord";
  matrixAddress = "https://${matrixDomain}";
  matrixDomain = "${hostName}.${suffix}";
  commonMautrixSettings = mode: {
    meta = {
      inherit mode;
    };
    homeserver = {
      domain = matrixDomain;
      address = matrixAddress;
      async_media = true;
    };
    appservice = {
      id = mode;
      bot = {
        username = "${mode}bot";
        displayname = "${mine.strings.capitalizeFirstLetter mode} bridge bot";
        avatar =
          if mode == "facebook"
          then "mxc://maunium.net/ygtkteZsXnGJLJHRchUwYWak"
          else "mxc://maunium.net/JxjlbZUlCPULEeHZSwleUXQv";
      };
      database = {
        type = "postgres";
        uri = "$MAUTRIX_META_APPSERVICE_DATABASE_URI";
      };
    };
    bridge = {
      double_puppet_server_map.${matrixDomain} = matrixAddress;
      login_shared_secret_map.${matrixDomain} = "as_token:$MAUTRIX_META_DOUBLE_PUPPET_SECRET";
      personal_filtering_spaces = true;
      username_template = "${mode}_{userid}";
      management_room_text.welcome = "Hello, I'm a ${mine.strings.capitalizeFirstLetter mode} bridge bot.";
      permissions = {
        "@mrnobody:${matrixDomain}" = "admin";
        "${matrixDomain}" = "user";
      };
      encryption = {
        allow = true;
        default = true;
        require = true;
        delete_keys = {
          dont_store_outbound = true;
          ratchet_on_decrypt = true;
          delete_fully_used_on_decrypt = true;
          delete_prev_on_new_session = true;
          delete_on_device_delete = true;
          periodically_delete_expired = true;
          delete_outdated_inbound = true;
        };
        verification_levels = {
          receive = "cross-signed-tofu";
          send = "cross-signed-tofu";
          share = "cross-signed-tofu";
        };
      };
    };
    logging = {
      min_level = "error";
      writers = lib.singleton {
        type = "stdout";
        format = "pretty-colored";
        time_format = " ";
      };
    };
  };
in {
  sops.secrets = {
    "matrix-appservice-discord/id" = {};
    "matrix-appservice-discord/token" = {};
  };
  sops.templates = {
    "mautrix-meta-facebook-postgresql-connection".content = ''
      MAUTRIX_META_APPSERVICE_DATABASE_URI="postgres://mautrix-meta-facebook:${config.sops.placeholder."postgresql/mautrix-meta-facebook"}@localhost/mautrix-meta-facebook?sslmode=disable"
      MAUTRIX_META_DOUBLE_PUPPET_SECRET="${config.sops.placeholder."synapse/double_puppet"}"
    '';
    "mautrix-meta-instagram-postgresql-connection".content = ''
      MAUTRIX_META_APPSERVICE_DATABASE_URI="postgres://mautrix-meta-instagram:${config.sops.placeholder."postgresql/mautrix-meta-instagram"}@localhost/mautrix-meta-instagram?sslmode=disable"
      MAUTRIX_META_DOUBLE_PUPPET_SECRET="${config.sops.placeholder."synapse/double_puppet"}"
    '';
    "matrix-appservice-discord-postgresql-connection".content = ''
      APPSERVICE_DISCORD_DATABASE_CONN_STRING="postgresql://matrix-appservice-discord:${config.sops.placeholder."postgresql/matrix-appservice-discord"}@localhost/matrix-appservice-discord?sslmode=disable"
      APPSERVICE_DISCORD_AUTH_BOT_TOKEN= "${config.sops.placeholder."matrix-appservice-discord/token"}"
      APPSERVICE_DISCORD_AUTH_CLIENT_I_D= "${config.sops.placeholder."matrix-appservice-discord/id"}"
    '';
  };

  environment.persistence."/persist".directories = [
    appserviceDiscordDir
  ];
  services.matrix-synapse.settings.app_service_config_files = [
    "${appserviceDiscordDir}/discord-registration.yaml"
  ];
  users = {
    users."matrix-appservice-discord" = {
      isSystemUser = true;
      group = "matrix-appservice-discord";
      extraGroups = ["matrix-appservice-discord-registration"];
      description = "Matrix-appservice-discord bridge user";
    };
    groups."matrix-appservice-discord" = {};
    groups."matrix-appservice-discord-registration".members =
      lists.optional config.services.matrix-synapse.enable "matrix-synapse";
  };
  systemd.services.matrix-appservice-discord = {
    preStart = mkAfter ''
      chown :matrix-appservice-discord-registration ${appserviceDiscordDir}/discord-registration.yaml

    '';
    serviceConfig = rec {
      DynamicUser = mkForce false;
      User = "matrix-appservice-discord";
      Group = User;
    };
  };
  services = {
    matrix-appservice-discord = rec {
      enable = true;
      environmentFile = config.sops.templates."matrix-appservice-discord-postgresql-connection".path;
      port = 29320;
      url = "http://localhost:${toString port}";
      settings = {
        logging = {
          console = "error";
          files = [];
        };
        bridge = {
          domain = matrixDomain;
          homeserverUrl = matrixAddress;
          enableSelfServiceBridging = true;
          adminMxid = "@mrnobody:${matrixDomain}";
        };
        database = {
          filename = "";
          connString = "$APPSERVICE_DISCORD_DATABASE_CONN_STRING";
        };
        auth = {
          botToken = "$APPSERVICE_DISCORD_AUTH_BOT_TOKEN";
          clientID = "$APPSERVICE_DISCORD_AUTH_CLIENT_I_D";
          usePrivilegedIntents = true;
        };
        room.defaultVisibility = "private";
      };
    };
    mautrix-meta.instances = {
      facebook = {
        enable = true;
        environmentFile = config.sops.templates."mautrix-meta-facebook-postgresql-connection".path;
        settings = recursiveUpdate (commonMautrixSettings "facebook") {
          appservice = rec {
            hostname = "localhost";
            port = 29319;
            address = "http://${hostname}:${toString port}";
          };
        };
      };
      instagram = {
        enable = true;
        environmentFile = config.sops.templates."mautrix-meta-instagram-postgresql-connection".path;
        settings = recursiveUpdate (commonMautrixSettings "instagram") {
          appservice = rec {
            hostname = "localhost";
            port = 29318;
            address = "http://${hostname}:${toString port}";
          };
        };
      };
    };
  };
}
