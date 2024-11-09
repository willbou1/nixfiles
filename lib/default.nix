{lib}:
with builtins;
with lib.attrsets;
with lib.strings; {
  strings = import ./strings.nix {lib = lib;};

  # Include everything in the current directory
  autoInclude = dir: blacklist: let
    files = dir:
      attrNames (filterAttrs (_: type: type == "regular") (readDir dir));
    directories = dir:
      attrNames (filterAttrs (_: type: type == "directory") (readDir dir));
    nixFiles = dir:
      filter (file: hasSuffix ".nix" file && file != "default.nix")
      (files dir);
    relevantContent = dir:
      map (e: dir + "/${e}") ((nixFiles dir) ++ (directories dir));
    filterContent = blacklist: content:
      filter (e: !(elem e blacklist)) content;
  in
    filterContent blacklist (relevantContent dir);
}
