{ pkgs, ... }:

{
    imports = [
        ./vifm-module.nix
    ];

    programs.vifm = {
        enable = true;
        vifmimg.enable = true;
        package = pkgs.vifm-full;
        config = {
            set = [
                "relativenumber"
                "vicmd=nvim"
                "syscalls"
                "confirm-=delete"
                "classify='  :dir:/,  :exe:,  :reg:,  :link:'"
                "classify+='  ::../::,  ::*.sh::,  ::*.[hc]pp::,  ::*.[hc]::,  ::/^copying|license$/::,  ::.git/,,*.git/::,  ::*.epub,,*.fb2,,*.djvu::,  ::*.pdf::,  ::*.htm,,*.html,,**.[sx]html,,*.xml::'"
                "classify+='  ::*.7z,,*.ace,,*.arj,,*.bz2,,*.cpio,,*.deb,,*.dz,,*.gz,,*.jar,,*.lzh,,*.lzma,,*.rar,,*.rpm,,*.rz,,*.tar,,*.taz,,*.tb2,,*.tbz,,*.tbz2,,*.tgz,,*.tlz,,*.trz,,*.txz,,*.tz,,*.tz2,,*.xz,,*.z,,*.zip,,*.zoo::'"
                "classify+='  ::*.bmp,,*.gif,,*.jpeg,,*.jpg,,*.ico,,*.png,,*.ppm,,*.svg,,*.svgz,,*.tga,,*.tif,,*.tiff,,*.xbm,,*.xcf,,*.xpm,,*.xspf,,*.xwd::'"
                "classify+='  ::*.aac,,*.anx,,*.asf,,*.au,,*.axa,,*.flac,,*.m2a,,*.m4a,,*.mid,,*.midi,,*.mp3,,*.mpc,,*.oga,,*.ogg,,*.ogx,,*.ra,,*.ram,,*.rm,,*.spx,,*.wav,,*.wma,,*.ac3::'"
                "classify+='  ::*.avi,,*.ts,,*.axv,,*.divx,,*.m2v,,*.m4p,,*.m4v,,.mka,,*.mkv,,*.mov,,*.mp4,,*.flv,,*.mp4v,,*.mpeg,,*.mpg,,*.nuv,,*.ogv,,*.pbm,,*.pgm,,*.qt,,*.vob,,*.wmv,,*.xvid::'"
                "classify+='  ::*.doc,,*.docx::,  ::*.xls,,*.xls[mx]::,  ::*.pptx,,*.ppt::'"
            ];
            filetype = [
                "*.csv,*.xlsx libreoffice %c %i"
                "<text/*>,*.sh nvim"
                "*.pdf zathura %c %i &"
                "*.epub zathura %c %i &"
                "<audio/*> mpv %c %i </dev/null &>/dev/null &"
                "<video/*> mpv %c %i </dev/null &>/dev/null &"
                "<image/*> feh %c %i </dev/null &>/dev/null &"
                "* xdg-open %c"
            ];
            fileviewer = [
                "<text/*>,*.sh,*.nix,*.xml env -uCOLORTERM bat --color always --wrap never --pager never %c -p"
                "*.zip,*.jar,*.war,*.ear,*.oxt zip -sf %c"
                "*.tgz,*.tar.gz tar -tzf %c"
                "*.tar.bz2,*.tbz2 tar -tjf %c"
                "*.tar.txz,*.txz xz --list %c"
                "*.tar tar -tf %c"
                "*.rar unrar v %c"
                "*.7z 7z l %c"
                "*/ lsd --color always --icon always"
                ".*/ lsd --color always --icon always"
                "* file -b %c"
            ];
            map = [
                "q ZQ"
            ];
            nnoremap = [
                "gg ggj"
                ". za"
                "F :FZFfind <cr>"
                "o :!dragon-drag-and-drop %f </dev/null &>/dev/null & <cr>"
                "I cw<c-a>"
                "cc cw<c-u>"
                "A cw"
                "w :view<cr>"
                "S :sort<cr>"
                "s :shell<cr>"
            ];
        };
        extraConfig = ''
            view
        '';
    };
}
