{
  lib,
  config,
  ...
}:
with lib; let
  hostName = config.networking.hostName;
  suffix = config.networking.suffix;
  matrixAddress = "https://${matrixDomain}";
  matrixDomain = "${hostName}.${suffix}";
  # database has moved to root
  # encryption has moved to root
  # double puppet is a section now
  commonMautrixSettings = mode: {
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
      };
      database = {
        type = "postgres";
        uri = "$DATABASE_URI";
      };
    };
    bridge = {
      double_puppet_server_map.${matrixDomain} = matrixAddress;
      login_shared_secret_map.${matrixDomain} = "as_token:$DOUBLE_PUPPET_SECRET";
      personal_filtering_spaces = true;
      username_template = "${mode}_{{.}}";
      management_room_text.welcome = "Hello, I'm a ${mine.strings.capitalizeFirstLetter mode} bridge bot.";
      permissions = {
        "@mrnobody:${matrixDomain}" = "admin";
        "${matrixDomain}" = "user";
      };

      encryption = {
        # idk how it's not the default considering it's matrix we're talking about
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
  # EX importing a module from unstable
  imports = [
    #"${unstable}/nixos/modules/services/matrix/mautrix-discord.nix"
  ];

  sops.templates = {
    "mautrix-meta-facebook-postgresql-connection".content = ''
      DATABASE_URI="postgres://mautrix-meta-facebook:${config.sops.placeholder."postgresql/mautrix-meta-facebook"}@localhost/mautrix-meta-facebook?sslmode=disable"
      DOUBLE_PUPPET_SECRET="${config.sops.placeholder."synapse/double_puppet"}"
    '';
    "mautrix-meta-instagram-postgresql-connection".content = ''
      DATABASE_URI="postgres://mautrix-meta-instagram:${config.sops.placeholder."postgresql/mautrix-meta-instagram"}@localhost/mautrix-meta-instagram?sslmode=disable"
      DOUBLE_PUPPET_SECRET="${config.sops.placeholder."synapse/double_puppet"}"
    '';
    "mautrix-discord-postgresql-connection".content = ''
      DATABASE_URI="postgres://mautrix-discord:${config.sops.placeholder."postgresql/mautrix-discord"}@localhost/mautrix-discord?sslmode=disable"
      DOUBLE_PUPPET_SECRET="${config.sops.placeholder."synapse/double_puppet"}"
    '';
  };

  services = {
    mautrix-discord = {
      enable = true;
      environmentFile = config.sops.templates."mautrix-discord-postgresql-connection".path;
      settings = recursiveUpdate (commonMautrixSettings "discord") {
        appservice.bot.username = "discordbot2";
      };
    };
    mautrix-meta.instances = {
      facebook = {
        enable = true;
        registerToSynapse = true;
        environmentFile = config.sops.templates."mautrix-meta-facebook-postgresql-connection".path;
        # TODO this will soon be the right way to configure discord too so keep an eye for that
        settings = let 
          legacy = commonMautrixSettings "facebook";
        in recursiveUpdate legacy {
          # 1. Clear out the legacy paths so they don't trip the validator
          appservice.database = null;
          bridge.username_template = null;
          bridge.double_puppet_server_map = null;
          bridge.login_shared_secret_map = null;
          bridge.encryption = null;

          # 2. Inject the brand new top-level blocks from the Go spec
          network.mode = "facebook";

          appservice.username_template = legacy.bridge.username_template;

          database = {
            type = legacy.appservice.database.type;
            uri = legacy.appservice.database.uri;
          };

          double_puppet = {
            servers = legacy.bridge.double_puppet_server_map;
            secrets = legacy.bridge.login_shared_secret_map;
            allow_discovery = legacy.bridge.double_puppet_allow_discovery or false;
          };

          encryption = legacy.bridge.encryption;
        };
      };
      instagram = {
        enable = true;
        registerToSynapse = true;
        environmentFile = config.sops.templates."mautrix-meta-instagram-postgresql-connection".path;
        settings = let 
          legacy = commonMautrixSettings "instagram";
        in recursiveUpdate legacy {
          # 1. Clear out the legacy paths so they don't trip the validator
          appservice.database = null;
          bridge.username_template = null;
          bridge.double_puppet_server_map = null;
          bridge.login_shared_secret_map = null;
          bridge.encryption = null;

          # 2. Inject the brand new top-level blocks from the Go spec
          network.mode = "instagram";

          appservice.username_template = legacy.bridge.username_template;

          database = {
            type = legacy.appservice.database.type;
            uri = legacy.appservice.database.uri;
          };

          double_puppet = {
            servers = legacy.bridge.double_puppet_server_map;
            secrets = legacy.bridge.login_shared_secret_map;
            allow_discovery = legacy.bridge.double_puppet_allow_discovery or false;
          };

          encryption = legacy.bridge.encryption;
        };
      };
    };
  };
}
