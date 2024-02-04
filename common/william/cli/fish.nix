{ pkgs, ... }:

{
    home.persistence."/persist/home/william".directories = [
        ".local/share/fish"
    ];
    programs.fish = {
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
            "uc" = "systemctl --user";
            "nd" = "nix develop --command fish";
            "w" = "curl -s wttr.in | less -RS";
            "ns" = "nix-shell --run fish";
            "ujc" = "journalctl --user";
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
            killall = ''
                pkill ".*$argv.*"
            '';
            bt = ''
                for i in (seq $argv[1])
                    cd ..
                end
            '';
        };
        interactiveShellInit = ''
            fish_vi_key_bindings
            set -e MESA_LOADER_DRIVER_OVERRIDE
            set -e __EGL_VENDOR_LIBRARY_FILENAMES
            '';
        plugins = with pkgs.fishPlugins; [
        {
            name ="ssh-agent";
            src = pkgs.fetchFromGitHub {
                owner = "danhper";
                repo = "fish-ssh-agent";
                rev = "fd70a2afdd03caf9bf609746bf6b993b9e83be57";
                hash = "sha256-e94Sd1GSUAxwLVVo5yR6msq0jZLOn2m+JZJ6mvwQdLs=";
            };
        }
        {
            name = "fzf";
            src = fzf-fish.src;
        }
        {
            name = "colored-man";
            src = colored-man-pages.src;
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
    };
}
