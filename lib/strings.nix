{lib, ...}:
with builtins;
with lib.strings; {
  removeChar = char: string:
    concatStrings (filter (c: c != char) (stringToCharacters string));

  capitalizeFirstLetter = string: let
    firstLetter = substring 0 1 string;
    restOfString = substring 1 ((stringLength string) - 1) string;
  in "${toUpper firstLetter}${restOfString}";
}
