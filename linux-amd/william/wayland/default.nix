{
  pkgs,
  lib,
  ...
}:
with lib; {
  imports = mine.autoInclude ./. [];
}
