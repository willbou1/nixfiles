{ ... }:

{
    accounts.email = {
        maildirBasePath = "Mail";
        accounts.personal = rec {
            primary = true;
            address = "willbou2@gmail.com";
            passwordCommand = "";

            imap.host = "mail.m7.rs";
            mbsync = {
                enable = true;
                create = "maildir";
                expunge = "both";
            };
            neomutt = {
                enable = true;
            };
            msmtp.enable = true;
            smtp.host = "mail.m7.rs";
            userName = address;
            realName = "William Boulanger";
        };
    };

    programs = {
        mbsync.enable = true;
        msmtp.enable = true;
    };
}
