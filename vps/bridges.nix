{ config, ... }:
with lib;
let
hostName = config.networking.hostName;
localMatrixAddress = "http://127.0.0.1:9009";
matrixDomain = "matrix.${hostName}";
commonMautrixSettings = mode: {
    inherit mode;
    homeserver = {
        domain = matrixDomain;
        address = localMatrixAddress;
    };
    appservice = {
        id = mode;
        bot = {
            username = "${mode}bot";
            displayname = "${mine.capitalizeFirstLetter mode} bridge bot";
            avatar = if mode == "facebook" then "mxc://maunium.net/ygtkteZsXnGJLJHRchUwYWak"
                else "mxc://maunium.net/JxjlbZUlCPULEeHZSwleUXQv";
        };
        database = {
            type = "postgres";
            uri = "$POSTGRES_CONNECTION_STRING";
        };
    };
    bridge = {
        username_template = "${mode}_{userid}";
        management_room_text.welcome = "Hello, I'm a ${mine.capitalizeFirstLetter mode} bridge bot.";
        permissions = {
            "@mrnobody:matrix.ourmiraculous.com" = "admin";
            "matrix.ourmiraculous.com" = "user";
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
        min_level = "critical";
        writers = lib.singleton {
            type = "stdout";
            format = "pretty-colored";
            time_format = " ";
        };
    };
};
in {
    sops.templates = {
        "mautrix_meta_facebook_postgresql_connection".content = ''
            POSTGRES_CONNECTION_STRING="postgres://mautrix_meta_facebook_user:${config.sops.placeholder."postgresql/mautrix_meta_facebook_user"}@localhost/mautrix_meta_facebook?sslmode=disable"
        '';
        "mautrix_meta_instagram_postgresql_connection".content = ''
            POSTGRES_CONNECTION_STRING="postgres://mautrix_meta_instagram_user:${config.sops.placeholder."postgresql/mautrix_meta_instagram_user"}@localhost/mautrix_meta_instagram?sslmode=disable"
        '';
    };

    services = {
        matrix-meta.instances = {
            facebook = {
                enable = true;
                dataDir = "../../srv/mautrix_meta_facebook";
                environmentFile = config.sops.templates."mautrix_meta_facebook_postgresql_connection".path;
                settings = (commonMautrixSettings "facebook") // {
                    appservice = rec {
                        hostname = "localhost";
                        port = 29319;
                        address = "http://${hostname}:${port}";
                    };
                };
            };
            instagram = {
                enable = true;
                dataDir = "../../srv/mautrix_meta_instagram";
                environmentFile = config.sops.templates."mautrix_meta_instagram_postgresql_connection".path;
                settings = (commonMautrixSettings "instagram") // {
                    appservice = rec {
                        hostname = "localhost";
                        port = 29318;
                        address = "http://${hostname}:${port}";
                    };
                };
            };
        };
    };
}
