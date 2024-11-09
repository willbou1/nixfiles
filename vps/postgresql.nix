{
  config,
  pkgs,
  ...
}:
with builtins; let
  users = map (u: u.name) config.services.postgresql.ensureUsers;
  passwordScript = user: let
    passwordFilePath = config.sops.secrets."postgresql/${user}".path;
  in ''
    DO $$
    DECLARE password TEXT;
    BEGIN
        password := trim(both from replace(pg_read_file('${passwordFilePath}'), E'\n', '''));
        EXECUTE format('ALTER ROLE "${user}" WITH PASSWORD '''%s''';', password);
    END $$;
  '';
  passwordScripts = concatStringsSep "\n" (map passwordScript users);
in {
  system.activationScripts.createPostgresqlDir.text = ''
    mkdir -p /srv/postgresql
    chown postgres:postgres /srv/postgresql
    chmod 750 /srv/postgresql
  '';
  sops = {
    secrets = let
      commonProps = {
        mode = "0440";
        owner = "postgres";
      };
    in {
      "postgresql/vaultwarden" =
        {
        }
        // commonProps;
      "postgresql/synapse" =
        {
        }
        // commonProps;
      "postgresql/mautrix-meta-facebook" =
        {
        }
        // commonProps;
      "postgresql/mautrix-meta-instagram" =
        {
        }
        // commonProps;
      "postgresql/matrix-appservice-discord" =
        {
        }
        // commonProps;
      "postgresql/grafana" =
        {
        }
        // commonProps;
    };
  };
  services.postgresql = {
    enable = true;
    dataDir = "/srv/postgresql/${config.services.postgresql.package.psqlSchema}";
    initdbArgs = [
      "--encoding=UTF8"
      "--locale=C"
    ];
    ensureDatabases = [
      "vaultwarden"
      "synapse"
      "mautrix-meta-facebook"
      "mautrix-meta-instagram"
      "matrix-appservice-discord"
      "grafana"
    ];
    ensureUsers =
      map (u: {
        name = u;
        ensureDBOwnership = true;
      }) [
        "synapse"
        "mautrix-meta-facebook"
        "mautrix-meta-instagram"
        "matrix-appservice-discord"
        "grafana"
        "vaultwarden"
      ];
  };

  systemd.services.postgresql.postStart = ''
    ${config.services.postgresql.package}/bin/psql -tA <<'EOF'
        ${passwordScripts}
    EOF
  '';
}
