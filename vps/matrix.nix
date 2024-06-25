{ pkgs, ... }:
let
hostName = config.networking.hostName;
elementAddress = "element.${hostName}";
matrixAddress = "matrix.${hostName}";
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
    };
};
in {
    sops.templates."synapse_postgresql_connection".content = ''
        database:
            name: psycopg2
            args:
                user: synapse-user
                password: ${config.sops.placeholder."postgresql/synapse_user"}
                dbname: synapse
                host: localhost
                port: 5432
                cp_min: 5
                cp_min: 10
    '';

    services.nginx.virtualHosts = {
        "Element" = {
            serverName = elementAddress;
            enableACME = true;
            forceSSL = true;
            locations."/" = {
                root = "${pkgs.element-web-custom}";
                index = "index.html";
                extraConfig = ''
                    add_header X-Frame-Options SAMEORIGIN;
                    add_header X-Content-Type-Options nosniff;
                    add_header X-XSS-Protection "1; mode=block";
                    add_header Content-Security-Policy "frame-ancestors 'none'";
                '';
            };
        };
        "SynapseAdmin" = {
            serverName = "synapse.${hostName}";
            enableACME = true;
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
        "Matrix" = {
            serverName = matrixAddress;
            enableACME = true;
            forceSSL = true;
            locations = {
                "~* ^(\/_matrix|\/_synapse\/client)" = {
                    proxyPass = "http://localhost:9009";
                    extraConfig = ''
                        proxy_set_header X-Forwarded-For $remote_addr;
                        proxy_set_header X-Forwarded-Proto $scheme;
                        proxy_set_header Host $host;
                        client_max_body_size 1024M;
                    '';
                };
                # Frontend proxy worker
                "^/_matrix/client/(r0|v3|unstable)/keys/upload" = {
                    proxyPass = "http://localhost:9009";
                    extraConfig = ''
                        proxy_set_header X-Forwarded-For $remote_addr;
                        proxy_set_header X-Forwarded-Proto $scheme;
                        proxy_set_header Host $host;
                        client_max_body_size 1024M;
                    '';
                };
            };
        };
    };

    services.matrix-synapse = {
        enable = true;
        dataDir = "/srv/matrix-synapse";
        extraConfigFiles = [
            config.sops.templates."synapse_postgresql_connection".path
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
        workers = {
            "federation_sender" = {};
            "generic_worker" = {};
        };
        settings = {
            server_name = matrixAddress;
            web_client_location = "https://${elementAddress}";
        };
    };
}
