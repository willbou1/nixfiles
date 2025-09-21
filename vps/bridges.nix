{
  lib,
  config,
  unstable,
  pkgs,
  ...
}:
with lib; let
  hostName = config.networking.hostName;
  suffix = config.networking.suffix;
  matrixAddress = "https://${matrixDomain}";
  matrixDomain = "${hostName}.${suffix}";
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
  imports = [
    "${unstable}/nixos/modules/services/matrix/mautrix-discord.nix"
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
        bridge.encryption = {
          # idk how it's not the default considering it's matrix we're talking about
          allow = true;
          default = true;
          require = true;
        };
      };
    };
    mautrix-meta.instances = {
      facebook = {
        enable = true;
        environmentFile = config.sops.templates."mautrix-meta-facebook-postgresql-connection".path;
        settings = recursiveUpdate (commonMautrixSettings "facebook") {
          network.mode = "facebook";
          encryption = {
            # idk how it's not the default considering it's matrix we're talking about
            allow = true;
            default = true;
            require = true;
          };
        };
      };
      instagram = {
        enable = true;
        environmentFile = config.sops.templates."mautrix-meta-instagram-postgresql-connection".path;
        settings = recursiveUpdate (commonMautrixSettings "instagram") {
          network.mode = "instagram";
          encryption = {
            # idk how it's not the default considering it's matrix we're talking about
            allow = true;
            default = true;
            require = true;
          };
        };
      };
    };
  };
}
