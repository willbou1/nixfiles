{ pkgs, config, ... }:
with builtins;
let
hostName = config.networking.hostName;
filtronRules = pkgs.writeText "filtronRules.json" (toJSON [
{
    name = "Search request";
    filters = ["Param:q" "Path=^(/|/search)$"];
    interval = 5;
    limit = 1;
    subrules = [
        {
            name = "Robo-agent limit";
            interval = 5;
            limit = 1;
            filters = ["Header:User-Agent=(curl|cURL|Wget|python-requests|Scrapy|FeedFetcher|Go-http-client)"];
            actions = [
                {
                    name = "block";
                    params = { message = "Rate limit exceeded"; };
                }
            ];
        }
        {
            name = "Bot limit";
            limit = 0;
            stop = true;
            filters = ["Header:User-Agent=(Googlebot|bingbot|Baiduspider|yacybot|YandexMobileBot|YandexBot|Yahoo! Slurp|MJ11bot|AhrefsBot|archive.org_bot|msnbot|MJ11bot|SeznamBot|linkdexbot|Netvibes|SMTBot|zgrab|James BOT)"];
            actions = [
                {
                    name = "block";
                    params = { "message" = "Rate limit exceeded"; };
                }
            ];
        }
        {
            name = "IP limit";
            interval = 5;
            limit = 1;
            stop = true;
            aggregations = ["Header:X-Forwarded-For"];
            actions = [
                {
                    name = "block";
                    params = { message = "Rate limit exceeded"; };
                }
            ];
        }
        {
            name = "RSS/JSON limit";
            interval = 5;
            limit = 1;
            stop = true;
            filters = ["Param:format=(csv|json|rss)"];
            actions = [
                {
                    name = "block";
                    params = { message = "Rate limit exceeded"; };
                }
            ];
        }
        {
            name = "User-agent limit";
            interval = 5;
            limit = 1;
            aggregations = ["Header:User-Agent"];
            actions = [
                {
                    name = "block";
                    params = { message = "Rate limit exceeded"; };
                }
            ];
        }
    ];
}
]);
FILTRON_ADDRESS = "127.0.0.1:4004";
MORTY_KEY = "t3aiMXUPClUwazELnTLl1b8U068EiToy8eVYJ8toIMhY";
morty = config.services.morty;
in {
    sops = {
        secrets = {
            "searx/secret_key" = {};
            "searx/image_proxy_key
        };
        templates = {
            "searx_secrets".content = ''
                SEARXNG_SECRET="${config.sops.placeholder."searx/secret_key"}"
            '';
        };
    };

    services.nginx.virtualHosts = {
        "Searx" = {
            serverName = "search.${hostName}";
            enableACME = true;
            forceSSL = true;
            locations = {
                "/" = {
                    return = "307 $scheme://$host/searx$request_uri";
                };
                "/searx" = {
                    proxyPass = "http://${FILTRON_ADDRESS}/";
                    extraConfig = ''
                        proxy_set_header   Host             $host;
                        proxy_set_header   Connection       $http_connection;
                        proxy_set_header   X-Real-IP        $remote_addr;
                        proxy_set_header   X-Forwarded-For  $proxy_add_x_forwarded_for;
                        proxy_set_header   X-Scheme         $scheme;
                        proxy_set_header   X-Script-Name    /searx;
                    '';
                };
                "/searx/static" = {
                    alias = "${config.services.searx.package}/share/static";
                };
                "/morty" = {
                    proxyPass = "http://${morty.listenAddress}:${morty.port}/";
                    extraConfig = ''
                        proxy_set_header   Host             $host;
                        proxy_set_header   Connection       $http_connection;
                        proxy_set_header   X-Real-IP        $remote_addr;
                        proxy_set_header   X-Forwarded-For  $proxy_add_x_forwarded_for;
                        proxy_set_header   X-Scheme         $scheme;
                    '';
                };
            };
        };
    };

    services = {
        searx = {
            enable = true;
            environmentFile = config.sops.templates."searx_secrets".path;
            runInUwsgi = false;
            settings = {
                general = {
                    debug = false;
                    instance_name = "searx";
                };
                engines = [
                    {
                        name = "brave";
                        disabled = true;
                    }
                    {
                        name = "yahoo";
                        disabled = false;
                    }
                    {
                        name = "erowid";
                        disabled = true;
                    }
                    {
                        name = "naver";
                        disabled = false;
                    }
                ];
                server = {
                    port = 8888;
                    bind_address = "127.0.0.1";
                    image_proxy = true;
                };
                result_proxy = {
                    url = "/morty";
                    key = MORTY_KEY;
                };
            };
        };
        morty = {
            enable = true;
            listenAddress = "127.0.0.1";
            port = 3000;
            key = MORTY_KEY;
        };
    };

    systemd.services.filtron = {
        enable = true;
        description = "Filtron instance";
        serviceConfig = {
            Type = "simple";
            Restart = "on-failure";
            RestartSec = 1;
            ExecStart = "${pkgs.filtron}/bin/filtron -listen ${FILTRON_ADDRESS} -rules ${filtronRules}";
        };
        after = [ "network.target" ];
        wantedBy = [ "multi-user.target" ];
    };
}
