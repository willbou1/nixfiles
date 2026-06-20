{
  lib,
  config,
  pkgs,
  ...
}:
with builtins; let
  myDwmBlocks =
    (pkgs.custom-dwmblocks.overrideAttrs {
      src = pkgs.fetchFromGitHub {
        owner = "willbou1";
        repo = "dwmblocks";
        rev = "83522dad7dfebecf5230704641c8a1f83756831e";
        hash = "sha256-1n/onlzW1ntAjsJf4WgiDJm8S2L+4yejz85zK0Dej7A=";
      };
      NIX_CFLAGS_COMPILE = [
        "-Wno-error=int-conversion"
        "-Wno-error=incompatible-pointer-types"
      ];
    })
    .override {
      conf = ''
        #define PATHTOCMD "${config.xdg.configHome}/dwmblocks/"

        #define CMD(cmd) PATHTOCMD #cmd ".sh"

        //Modify this file to change what commands output to your statusbar, and recompile using the make command.
        static const Block blocks[] = {
        	/*Icon*/	/*Command*/		/*Update Interval*/	/*Update Signal*/
        	{"🧠 ", CMD("cpu"),	        1,			        1},
        	{"🌡️ ", CMD("temp"),	    1,			        8},
        	{"📝 ", CMD("memory"),	    1,			        2},
        	{"🔊 ", CMD("volume"),	    1,      	        10},
        	{"🎤 ", CMD("mic"),	        1,      	        13},
        	{"🌐 ", CMD("network"),     30,      	        3},
        	{"🎧 ", CMD("bluetooth"),   1,      	        4},
        	{"🖥️ ", CMD("backlight"),   0,      	        5},
        	{"⌨️ ",  CMD("keylayout"),   1,      	        6},
        	{"📅 ", CMD("date"),	    60,			        7},
        	{"📻",  CMD("player"),	    0,			        11},
        	{"💿",  CMD("disk"),	    0,			        12},
        	{"🔌",  CMD("power"),	    0,			        9},
        };

        //sets delimeter between status commands. NULL character ('\0') means no delimeter.
        static char delim[] = "  ";
        static unsigned int delimLen = 2;
        static unsigned int padding = 2;

        static double timeout = 0.15;
      '';
    };
in {
  home.packages = [myDwmBlocks];
  xdg.configFile =
    listToAttrs
    (map
      (n: {
        name = "dwmblocks/${n}";
        value.source =
          if lib.hasSuffix "sh" n
          then
            pkgs.writeShellScript n
            (''
                terminal="${config.home.terminal}"
              ''
              + (readFile (./blocks + "/${n}")))
          else ./blocks + "/${n}";
      })
      (attrNames (readDir ./blocks)));
}
