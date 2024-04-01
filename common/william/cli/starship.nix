{ lib, pkgs, ... }:
with lib;
let
    settings = {
        add_newline = false;
        scan_timeout = 10;
        command_timeout = 750;
        format = "[⌜ ](bold cyan)[ ](bold blue)$username$hostname$localip$shlvl$singularity$kubernetes$directory$vcsh$git_branch$git_commit$git_state$git_metrics$git_status$hg_branch$docker_context$package$buf$c$cmake$cobol$container$daml$dart$deno$dotnet$elixir$elm$erlang$golang$haskell$helm$java$julia$kotlin$lua$nodejs$ocaml$perl$php$pulumi$purescript$python$rlang$red$ruby$rust$scala$swift$terraform$vlang$vagrant$zig$nix_shell$conda$spack$memory_usage$aws$nim$gcloud$openstack$azure$env_var$crystal[ ]()$fill[  ]()$time[ ⌝](bold cyan)$line_break[⌞ ](bold cyan)$sudo$cmd_duration$jobs$battery$status$shell$character";
        right_format = "[ ⌟](bold cyan)";
        git_branch.format = "[$symbol$branch(:$remote_branch)]($style) ";
        cmd_duration.format = "[$duration]($style) ";
        hostname = {
            ssh_symbol = "";
            format = "[$ssh_symbol$hostname]($style) ";
        };
        username.format = "[$user]($style)@";
        character = {
            vimcmd_symbol = "[N](bold green)";
            vimcmd_replace_one_symbol = "[_](bold purple)";
            vimcmd_replace_symbol = "[_](bold purple)";
            vimcmd_visual_symbol = "[=](bold yellow)";
        };
        directory = {
            format = "[$read_only]($read_only_style)[$path]($style) ";
            truncation_symbol = "…/";
            truncation_length = 4;
        };
        haskell.format = "[$symbol($version )]($style)";
        java.format = "[\${symbol}(\${version} )]($style)";
        nix_shell.format = "[$symbol$state( \($name\))]($style) ";
        sudo = {
            format = "[$symbol]($style) ";
            symbol = "#";
            disabled = false;
        };
        status.disabled = false;
        fill = {
            symbol = "⋅";
            style = "dimmed cyan";
        };
        time = {
            format = "[$time](gray)";
            time_format = "%I:%M";
            disabled = false;
        };
        rust.format = "[$symbol($version )]($style)";
        lua.format = "[$symbol($version )]($style)";
        c.format = "[$symbol($version(-$name) )]($style)";
    };
    settingsFormat = pkgs.formats.toml {};
    privateSettings = settingsFormat.generate "starship_private.toml" (settings // {
        right_format = "[ PRIVATE ⌟](bold cyan)";
    });
    ttySettings = settingsFormat.generate "starship_tty.toml" (settings // {
        format = "$username$hostname$localip$shlvl$singularity$kubernetes$directory$vcsh$git_branch$git_commit$git_state$git_metrics$git_status$hg_branch$docker_context$package$buf$c$cmake$cobol$container$daml$dart$deno$dotnet$elixir$elm$erlang$golang$haskell$helm$java$julia$kotlin$lua$nodejs$ocaml$perl$php$pulumi$purescript$python$rlang$red$ruby$rust$scala$swift$terraform$vlang$vagrant$zig$nix_shell$conda$spack$memory_usage$aws$nim$gcloud$openstack$azure$env_var$crystal$sudo$cmd_duration$jobs$battery$status$shell$character";
        right_format = "";
        character = {
            success_symbol = "[>](bold green)";
            error_symbol = "[X](bold red)";
        };
        directory = {
            read_only = "RO ";
            format = "[$read_only]($read_only_style)[$path]($style) ";
            truncation_symbol = "../";
            truncation_length = 5;
        };
        git_branch.symbol = "B ";
        status = {
            disabled = false;
            symbol = "x";
        };
    });
in {
    programs.starship = {
        enable = true;
        enableFishIntegration = true;
        inherit settings;
    };
    xdg.configFile."starship_private.toml".source = privateSettings;
    xdg.configFile."starship_tty.toml".source = ttySettings;
    programs.fish.interactiveShellInit = mkAfter ''
        tty | grep tty > /dev/null && set -x STARSHIP_CONFIG ~/.config/starship_tty.toml || set -x STARSHIP_CONFIG ~/.config/starship.toml
    '';
}
