{lib, ...}:
with lib; {
  imports = mine.autoInclude ./. [];

  home.persistence."/persist".directories = [
    "priv"
  ];
}
