{ inputs, lib, config, pkgs, ... }: 

{
	networking.networkmanager.wifiAddress = "10.0.0.160/24,10.0.0.1";
	networking.hostName = "linux-amd";
	environment = {
        persistence."/persist".directories = [
            "/var/lib/jellyfin"
            "/var/cache/jellyfin"
        ];
        systemPackages = with pkgs; [
            jellyfin-ffmpeg
        ];
    };
	networking.firewall = {
        allowedTCPPorts = [ 21 ];
        allowedTCPPortRanges = [
            { from = 2000; to = 2030; }
        ];
    };
    services = {
		deluge.web.enable = true;
        jellyfin = {
            enable = true;
            openFirewall = true;
        };
        vsftpd = {
            enable = true;
            localUsers = true;
            writeEnable = true;
            chrootlocalUser = true;
            allowWriteableChroot = true;
            #forceLocalDataSSL = true;
            #forceLocalLoginsSSL = true;
            userlist = [
                "william"
                "laurice"
                "marc"
                "gabriel"
                "alexandre"
            ];
            extraConfig = ''
                pasv_enable=Yes
                pasv_max_port=2030
                pasv_min_port=2000
            '';
        };
    };
}
