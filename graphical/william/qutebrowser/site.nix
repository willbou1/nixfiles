{
  lib,
  config,
  ...
}:
with builtins;
with lib; let
  cfg = config.programs.qutebrowser;
in {
  options.programs.qutebrowser.domainSettings = mkOption {
    type = types.attrsOf (types.attrsOf types.anything);
    default = {};
    description = ''
      Per-domain options to add to qutebrowser {file}`config.py` file.
      See <https://qutebrowser.org/doc/help/settings.html>
      for options.
    '';
    example = literalExpression ''
      {
        "https://netflix.com" = {
          content.notifications.enable = false;
        };
        "https://facebook.com" = {
          content.notifications.enable = false;
        };
      }
    '';
  };
  config.programs.qutebrowser.extraConfig = let
    formatValue = v:
      if v == null
      then "None"
      else if isBool v
      then
        (
          if v
          then "True"
          else "False"
        )
      else if isString v
      then ''"${v}"''
      else if isList v
      then "[${concatStringsSep ", " (map formatValue v)}]"
      else toString v;
    formatDomainBlock = n: v: let
      formatDomainLine = d: o: on: ov:
        if isAttrs ov
        then
          concatStringsSep "\n"
          (mapAttrsToList (formatDomainLine d "${o}${on}.") ov)
        else ''config.set("${o}${on}", ${formatValue ov}, "${d}")'';
    in
      concatStringsSep "\n" (mapAttrsToList (formatDomainLine n "") v);
  in
    concatStringsSep "\n" (mapAttrsToList formatDomainBlock cfg.domainSettings);
}
