let
  packageOverlay = path: final: prev: let
    dirContents = builtins.readDir path;
    genPackage = name: {
      inherit name;
      value = final.callPackage (path + "/${name}") {};
    };
    names = builtins.attrNames dirContents;
  in
    builtins.listToAttrs (map genPackage names);
in rec {
  overlay = packageOverlay ../pkgs;
  #nurOverlay = packageOverlay ../devel/nur-packages/pkgs;
}
