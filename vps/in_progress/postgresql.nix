{ config, pkgs, ... }:
let
users = map (u: u.name) config.services.postgresql.ensureUsers;
passwordScript = user: let
    passwordFilePath = config.sops.secrets."postgresal/${user}".path;
in ''
DO $$
DECLARE password TEXT;
BEGIN
    password := trim(both from replace(pg_read_file('${passwordFilePath}'), E'\n', '''));
    EXECUTE format('ALTER ROLE ${user} WITH PASSWORD '''%s''';', password);
END $$;
'';
passwordScripts = concatStringsSep "\n" (map passwordScript users);
in {
    sops = {
        secrets = {
            "postgresql/synapse_user" = {};
            "postgresql/mautrix_meta_facebook_user" = {};
            "postgresql/mautrix_meta_instagram_user" = {};
            "postgresql/mx_puppet_discord_user" = {};
            "postgresql/grafana_user" = {};
        };
    };
    services.postgresql = {
        enable = true;
        ensureDatabases = [
            "synapse"
            "mautrix_meta_facebook"
            "mautrix_meta_instagram"
            "mx_puppet_discord"
            "grafana"
        ];
        ensureUsers = [
            { name = "synapse_user"; }
            { name = "mautrix_meta_facebook_user"; }
            { name = "mautrix_meta_instagram_user"; }
            { name = "mx_puppet_discord_user"; }
            { name = "grafana_user"; }
        ];
    };

    systemd.services.postgresql.postStart = ''
        ${config.services.postgresql.package}/bin/psql -tA <<'EOF'
            ${passwordScripts}
        EOF
    '';
}
