{config, ...}:
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
  databases = [
    "vaultwarden" "synapse" "mautrix-meta-facebook" "mautrix-meta-instagram"
    "mautrix-discord" "grafana"
  ];
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
    in listToAttrs (map (d: {
      name = "postgresql/${d}";
      value = commonProps;
    }) databases);
  };
  services.postgresql = {
    enable = true;
    dataDir = "/srv/postgresql/${config.services.postgresql.package.psqlSchema}";
    initdbArgs = [
      "--encoding=UTF8"
      "--locale=C"
    ];
    ensureDatabases = databases;
    ensureUsers =
      map (u: {
        name = u;
        ensureDBOwnership = true;
      }) databases;
  };

  systemd.services.postgresql.postStart = ''
    ${config.services.postgresql.package}/bin/psql -tA <<'EOF'
        ${passwordScripts}
    EOF
  '';
}
