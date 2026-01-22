{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.programs.qalc;
  mkXMLString = content: ''
    <?xml version="1.0"?>
    <QALCULATE version="${cfg.package.version}">
    ${content}
    </QALCULATE>
  '';
  functionsXML =
    mkXMLString
      (strings.concatMapAttrsStringSep "\n"
        (k: v: ''
          <function>
            <names>${k}</names>
            <expression>${v}</expression>
          </function>
        '')
        cfg.functions
      );
  variablesXML =
    mkXMLString
      (strings.concatMapAttrsStringSep "\n"
        (k: v: ''
          <variable>
            <names>${k}</names>
            <value>${v}</value>
          </variable>
        '')
        cfg.variables
      );
in {
  options.programs.qalc = {
    enable = mkEnableOption "Qalc - Perform calculations with physical units";

    package = mkOption {
      type = types.package;
      default = pkgs.libqalculate;
      defaultText = literalExpression "pkgs.libqalculate";
      description = ''
        Qalc package to install.
      '';
    };

    config = mkOption {
      type = with types; nullOr (types.submodule {
        options = {
          General = mkOption {
            type = attrsOf (either bool int);
            default = {};
          };
          Mode = mkOption {
            type = attrsOf (either bool int);
            default = {};
          };
        };
      });
      default = null;
      description = ''
        Configuration written to
        {file}`$XDG_CONFIG_HOME/qalculate/qalc.cfg`. See
        <https://qalculate.github.io/manual/qalc.html>
        for the documentation.
      '';
    };

    functions = mkOption {
      type = with types; nullOr (attrsOf str);
      default = null;
      description = ''
        Functions to declare:
        {
          "name" = "expression";
        }
      '';
    };

    variables = mkOption {
      type = with types; nullOr (attrsOf str);
      default = null;
      description = ''
        Variables to declare:
        {
          "name" = "value";
        }
      '';
    };
  };
  config = mkIf cfg.enable {
    home.packages = [cfg.package];

    xdg = {
      dataFile = {
        "qalculate/definitions/functions.xml" = mkIf (cfg.functions != null) {
          text = functionsXML;
        };
        "qalculate/definitions/variables.xml" = mkIf ( cfg.variables != null) {
          text = variablesXML;
        };
      };

      configFile."qalculate/qalc.cfg" = mkIf (cfg.config != null)
        {
          text = (with lib.generators; toINI
            {
              mkKeyValue = mkKeyValueDefault {
                mkValueString = v:
                  if      v == false then "0"
                  else if v == true  then "1"
                  else               mkValueStringDefault v;
              } "=";
            }
            (recursiveUpdate cfg.config {
              General = {
                save_config = false;
                save_definitions = (isNull cfg.variables) && (isNull cfg.functions);
              };
            })
          );
        };
    };
  };
}
