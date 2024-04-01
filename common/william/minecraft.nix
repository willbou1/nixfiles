{ lib, config, pkgs, inputs, ... }:
with builtins;
with lib;
with config.lib.stylix.colors.withHashtag;
let
launcherPath = ".local/share/minecraft";
accountUuid = "b94d6f96-5516-4fbf-ba97-994ed503ee12";
baseAccountConfig = pkgs.writeText "accounts.json" (toJSON [{
        uuid = mine.strings.removeChar "-" accountUuid;
        displayName = "Shadow_710";
        tokenType = "Bearer";
        userid = "ab559897-57ef-4f93-b7ed-3e258f7cfa26";
        type = "microsoft";
        }]);
baseConfig = pkgs.writeText ".hmcl.json" (toJSON {
        commonpath = "${config.home.homeDirectory}/${launcherPath}";
        commonDirType = "CUSTOM";
        theme = base0A;
        titleTransparent = false;
        backgroundType = "TRANSLUCENT";
        fontSize = config.stylix.fonts.sizes.terminal;
        launcherFontFamily = config.stylix.fonts.serif.name;
        selectedAccount = "$GLOBAL:microsoft:${accountUuid}";
        configurations.Default.global = {
            gameDir = "${config.home.homeDirectory}/${launcherPath}";
            gameDirType = 2;
        };
    });
in {
    sops.secrets = {
        "minecraft/access_token" = {};
        "minecraft/refresh_token" = {};
        "minecraft/not_after" = {};
    };

    home.persistence."/persist/home/william" = {
        directories = [
            launcherPath
        ];
        files = [
            ".hmcl.json"
        ];
    };

    # accept licenses
    xdg.dataFile."hmcl/config.json" = {
        text = toJSON {
            agreementVersion = 1;
            platformPromptVersion = 1;
            multiplayerRelay = false;
            multiplayerAgreementVersion = 1;
        };
        force = true;
    };

    # merge real config with base config (base config has precedance)
    home.activation.hmcl = lib.hm.dag.entryAfter ["writeBoundary"] ''
        secPath="$XDG_RUNTIME_DIR/secrets/minecraft"
        accessToken="$(cat $secPath/access_token)"
        refreshToken="$(cat $secPath/refresh_token)"
        notAfter="$(cat $secPath/not_after)"
        accountParams="{accessToken: \"$accessToken\", refreshToken: \"$refreshToken\", notAfter: \"$notAfter\"}"
        mkdir -p ~/.local/share/hmcl
        ${pkgs.jq}/bin/jq ".[0] += $accountParams" ${baseAccountConfig} > ~/.local/share/hmcl/accounts.json

        [ -f ~/.hmcl.json ] || cat ${baseConfig} > ~/.hmcl.json
        newConfig="$(${pkgs.jq}/bin/jq -r -s '.[0] * .[1]' ~/.hmcl.json ${baseConfig})"
        echo "$newConfig" > ~/.hmcl.json
    '';

    home.packages = with pkgs; [
        hmcl
    ];
}
