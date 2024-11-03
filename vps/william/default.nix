{ lib, ... }:
with lib;
{
    imports = mine.autoInclude ./. [];

    home.persistence."/persist/home/william".directories = [
        "priv"
    ];
}
