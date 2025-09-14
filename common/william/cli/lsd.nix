{
  programs.lsd = {
    enable = true;
    settings = {
      date = "+%b %d %H:%M %y";
      indicators = true;
      sorting = {
        column = "time";
        reverse = true;
        dir-grouping = "first";
      };
    };
  };
}
