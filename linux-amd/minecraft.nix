let
    serverPath = "/var/lib/minecraft";
in {
	environment.persistence."/persist".directories = [
        serverPath
    ];
    services.minecraft-server = {
        enable = true;
        dataDir = serverPath;
        declarative = true;
        eula = true;
        openFirewall = true;
        serverProperties = {
            #white-list = true;
            gamemode = "survival";
            enable-command-block = true;
            pvp = false;
            difficulty = "easy";
            max-players = 5;
        };
#        whitelist = {
#            Shadow_710 = "b94d6f96-5516-4fbf-ba97-994ed503ee12";
#        };
    };    
}
