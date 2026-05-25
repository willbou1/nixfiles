{
  config,
  lib,
  ...
}: {
  home.persistence."/persist".directories = [
    "Mail"
  ];
  sops.secrets = {
    "mail_pass/willbou2" = {};
    "mail_pass/willbou1" = {};
  };
  accounts.email = {
    maildirBasePath = "Mail";
    accounts = {
      gmail = rec {
        primary = true;
        address = "willbou2@gmail.com";
        passwordCommand = "cat ${config.sops.secrets."mail_pass/willbou2".path}";

        imap.host = "imap.gmail.com";
        mbsync = {
          enable = true;
          create = "maildir";
          expunge = "both";
        };
        smtp.host = "smtp.gmail.com";

        msmtp.enable = true;
        mu.enable = true;

        userName = address;
        realName = "William Boulanger";
      };

      # MSFT and their damn OAuth stuff... makes me wanna shoot myself
      # This will be unsable with isync so I just forwarded hotmail to gmail
      hotmail = rec {
        enable = false;
        primary = false;
        address = "willbou1@hotmail.com";
        passwordCommand = "cat ${config.sops.secrets."mail_pass/willbou1".path}";

        imap = {
          host = "outlook.office365.com";
          port = 993;
        };
        smtp = {
          host = "smtp-mail.outlook.com";
          port = 587;
          tls.enable = true;
        };
        mbsync = {
          enable = true;
          create = "maildir";
          expunge = "both";
        };

        msmtp.enable = true;
        mu.enable = true;

        userName = address;
        realName = "William Boulanger";
      };
    };
  };

  services.mbsync.enable = true;
  programs = {
    msmtp.enable = true;
    mbsync.enable = true;
    mu.enable = true;
  };
}
