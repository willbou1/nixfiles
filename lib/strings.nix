{ lib, ... }:
with lib.strings;
{
    removeChar = char: string:
        concatStrings ((filter (c: c != char) (stringToCharacters string)));
}
