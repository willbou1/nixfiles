pkgs: {
	      enable = true;
	      shellAliases = {
		      "v" = "vifm";
		      "cat" = "bat";
		      "p" = "procs";
		      "n" = "ncpamixer";
		      "mutt" = "neomutt";
		      "t" = "trash";
		      "hlc" = "hyprctl";
		      "grep" = "rg";
		      "clear" = "command clear; fish_greeting";
	      };
	      functions = {
			      #fastfetch | lolcat -a -s 1000 -d 8
		      fish_greeting = ''
			      fastfetch
			      '';
		      cp = ''
			      command cp $argv & progress -mp $last_pid
			      '';
		      mv = ''
			      command mv $argv & progress -mp $last_pid
			      '';
		      dd = ''
			      command dd $argv & progress -mp $last_pid
			      '';
	      };
	      interactiveShellInit = ''
		      fish_vi_key_bindings
		      '';
	      plugins = [
		      {
			      name = "fzf";
			      src = pkgs.fishPlugins.fzf-fish.src;
		      }
		      {
			      name = "colored-man";
			      src = pkgs.fishPlugins.colored-man-pages.src;
		      }
		      {
			      name = "extract";
                  src = pkgs.fetchFromGitHub {
                      owner = "oh-my-fish";
                      repo = "plugin-extract";
                      rev = "5d05f9f15d3be8437880078171d1e32025b9ad9f";
                      hash = "sha256-hFM8uDHDfKBVn4CgRdfRaD0SzmVzOPjfMxU9X6yATzE=";
                  };
		      }
	      ];
      }
