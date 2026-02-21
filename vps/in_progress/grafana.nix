{config, ...}: let
  hostName = config.networking.hostName;
  databasePasswordFilePath = config.sops.secrets."postgresql/grafana_user".path;
in {
  services = {
    nginx.virtualHosts = {
      "Grafana" = {
        serverName = "grafana.${hostName}";
        enableACME = true;
        forceSSL = true;
        locations = {
          "/" = {
            proxyPass = "http://localhost:4567/";
            extraConfig = ''
              proxy_set_header Host $http_host;
            '';
          };
          "/api/live" = {
            proxyPass = "http://localhost:4567/";
            extraConfig = ''
              proxy_http_version 1.1;
              rewrite  ^/(.*)  /$1 break;
              proxy_set_header Upgrade $http_upgrade;
              proxy_set_header Connection $connection_upgrade;
              proxy_set_header Host $http_host;
            '';
          };
        };
      };
    };

    grafana = {
      enable = true;
      dataDir = "/srv/grafana";
      settings = {
        database = {
          type = "postgres";
          host = "127.0.0.1:5432";
          name = "grafana";
          user = "grafana_user";
          password = "$__file{${databasePasswordFilePath}}";
        };
      };
    };
  };
}
