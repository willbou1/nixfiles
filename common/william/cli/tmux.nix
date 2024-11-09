{...}: {
  programs.tmux = {
    enable = true;
    mouse = true;
    prefix = "C-a";
    keyMode = "vi";
    escapeTime = 0;
    extraConfig = ''
      set -g focus-events on

      bind h select-pane -L
      bind j select-pane -D
      bind k select-pane -U
      bind l select-pane -R
      bind r source-file ~/.config/tmux/tmux.conf

      set -g default-terminal "tmux-256color"
      set -ag terminal-overrides ",xterm-256color:RGB"

      is_vim="ps -o state= -o comm= -t '#{pane_tty}' | grep -iqE '^[^TXZ ]+ +(\\S+\\/)?g?(view|n?vim?x?)(diff)?$'"

      bind-key -n 'C-h' if-shell "$is_vim" 'send-keys C-h' { if -F '#{pane_at_left}' ''' 'select-pane -L' }
      bind-key -n 'C-j' if-shell "$is_vim" 'send-keys C-j' { if -F '#{pane_at_bottom}' ''' 'select-pane -D' }
      bind-key -n 'C-k' if-shell "$is_vim" 'send-keys C-k' { if -F '#{pane_at_top}' ''' 'select-pane -U' }
      bind-key -n 'C-l' if-shell "$is_vim" 'send-keys C-l' { if -F '#{pane_at_right}' ''' 'select-pane -R' }

      bind-key -T copy-mode-vi 'C-h' if -F '#{pane_at_left}' ''' 'select-pane -L'
      bind-key -T copy-mode-vi 'C-j' if -F '#{pane_at_bottom}' ''' 'select-pane -D'
      bind-key -T copy-mode-vi 'C-k' if -F '#{pane_at_top}' ''' 'select-pane -U'
      bind-key -T copy-mode-vi 'C-l' if -F '#{pane_at_right}' ''' 'select-pane -R'

      bind -n 'M-h' if-shell "$is_vim" 'send-keys M-h' 'resize-pane -L 1'
      bind -n 'M-j' if-shell "$is_vim" 'send-keys M-j' 'resize-pane -D 1'
      bind -n 'M-k' if-shell "$is_vim" 'send-keys M-k' 'resize-pane -U 1'
      bind -n 'M-l' if-shell "$is_vim" 'send-keys M-l' 'resize-pane -R 1'

      bind-key -T copy-mode-vi M-h resize-pane -L 1
      bind-key -T copy-mode-vi M-j resize-pane -D 1
      bind-key -T copy-mode-vi M-k resize-pane -U 1
      bind-key -T copy-mode-vi M-l resize-pane -R 1
    '';
  };
}
