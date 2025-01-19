{
  programs.qutebrowser = {
    keyBindings.normal = {
      ",m" = "spawn nmpv {url}";
      ",M" = "hint --rapid links spawn nmpv {hint-url}";
    };
  };
}
