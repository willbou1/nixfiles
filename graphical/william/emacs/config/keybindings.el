;; -*- lexical-binding: t; -*-

(require 'use-package)

;; ------------------------------------ Evil -----------------------------------
(use-package
  undo-tree
  :init
  (make-directory "~/.config/emacs/undotree/" t)
  :config
  (setq undo-tree-history-directory-alist '(("." . "~/.config/emacs/undotree")))
  (global-undo-tree-mode 1))

(use-package
  evil
  :after undo-tree
  :init
  (setq evil-want-C-u-scroll t
        evil-undo-system 'undo-tree
        evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package
  evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package
  evil-escape
  :after evil
  :config
  (evil-escape-mode 1)
  (setq-default evil-escape-key-sequence "tn"))

(use-package
  evil-mc
  :after evil
  :config
  (evil-mc-mode 1))

(use-package
  evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package
  which-key
  :config
  (setq which-key-idle-delay 0.8
        which-key-idle-secondary-delay 0.05)
  (which-key-mode))

;; ---------------------------------- General ----------------------------------
(use-package
  general
  :after evil
  :config
  (general-create-definer define-normal-key
    :keymaps 'override
    :states '(normal visual motion))

  (general-create-definer define-visual-key
    :keymaps 'override
    :states '(visual))

  (general-define-key
    :keymaps 'override
    "C-S-c" '(clipboard-kill-ring-save :which-key "Copy")
    "C-S-v" '(clipboard-yank :which-key "Paste")
   )

  (general-define-key
    :states '(normal visual insert)
    "C-h" #'sp-kill-hybrid-sexp
    "C-k" #'sp-kill-sexp
    "C-a" #'evil-mc-make-all-cursors
    "C-n" #'evil-mc-make-and-goto-next-match
    "C-p" #'evil-mc-make-and-goto-prev-match
    "C-x" #'evil-mc-make-cursor-here
    "C-b" #'evil-mc-undo-all-cursors
    )

  (defun my-indent-after-paste (beg end)
    "Auto-indent region after pasting."
    (when (and (derived-mode-p 'prog-mode 'text-mode) ; adjust modes as needed
               (not (region-active-p)))
      (indent-region beg end)))

  (advice-add 'yank :after
              (lambda (&rest args)
		(my-indent-after-paste (mark t) (point))))

  (let ((speed 3))
    (defhydra +hydra-resize-window (:hint nil)
              ("<left>" (evil-window-left 1) "←")
              ("<right>" (evil-window-right 1) "→")
              ("<down>" (evil-window-down 1) "↓")
              ("<up>" (evil-window-up 1) "↑")

              ("h" (evil-window-left 1) "←")
              ("l" (evil-window-right 1) "→")
              ("j" (evil-window-down 1) "↓")
              ("k" (evil-window-up 1) "↑")

              ("H" (evil-window-increase-width speed) "↑ width")
              ("L" (evil-window-decrease-width speed) "↓ width")
              ("J" (evil-window-increase-height speed) "↑ height")
              ("K" (evil-window-decrease-height speed) "↓ height")

              ("v" evil-window-vsplit "||")
              ("s" evil-window-split "==")
              ("e" evil-window-exchange "⮂")
              ("i" delete-other-windows "+")
              ("m" delete-other-windows "Max")
              ("b" balance-windows "50/50")
              ("d" evil-window-delete "x")
              ("r" window-layout-rotate-clockwise "⟳")
              ("R" window-layout-rotate-anticlockwise "⟲")
              ("w" ace-window "Ace"))

    (defhydra +hydra-resize-window-oneshot (:hint nil :exit t)
              ("<left>" (evil-window-left 1) "←")
              ("<right>" (evil-window-right 1) "→")
              ("<down>" (evil-window-down 1) "↓")
              ("<up>" (evil-window-up 1) "↑")

              ("h" (evil-window-left 1) "←")
              ("l" (evil-window-right 1) "→")
              ("j" (evil-window-down 1) "↓")
              ("k" (evil-window-up 1) "↑")

              ("H" (evil-window-increase-width speed) "↑ width")
              ("L" (evil-window-decrease-width speed) "↓ width")
              ("J" (evil-window-increase-height speed) "↑ height")
              ("K" (evil-window-decrease-height speed) "↓ height")

              ("v" evil-window-vsplit "||")
              ("s" evil-window-split "==")
              ("e" evil-window-exchange "⮂")
              ("i" delete-other-windows "+")
              ("m" delete-other-windows "Max")
              ("b" balance-windows "50/50")
              ("d" evil-window-delete "x")
              ("r" window-layout-rotate-clockwise "⟳")
              ("R" window-layout-rotate-anticlockwise "⟲")
              ("w" ace-window "Ace")))

  (define-normal-key
    :prefix "SPC"
    ":" '(helm-M-x					:which-key "M-x")
    ";" '(eval-expression				:which-key "Eval")
    "," '(helm-buffers-list				:which-key "Find buffer")
    "." '(helm-projectile				:which-key "Find file")
    "g" '(magit						:which-key "Magit")
    "/" '((lambda () (term "fish"))				:which-key "Terminal")
    "f" '(helm-do-grep-ag				:which-key "Find")
    "s" '(helm-yas-complete				:which-key "Snippet")
    "r" '(helm-tramp					:which-key "Tramp")
    "d" '(dired						:which-key "Dired")
    "w" '(+hydra-resize-window-oneshot/body	:which-key "Window oneshot")
    "W" '(+hydra-resize-window/body			:which-key "Window")
    "<backspace>" '(dashboard-open :which-key "Dashboard")
    "TAB" '(ace-window :which-key "Other window")
    "SPC" '((lambda () (interactive)
	      (switch-to-buffer (other-buffer))) :which-key "Other buffer")
    "p"  '(:prefix-command project-prefix-map		:which-key "Project")
    "h"  '(:prefix-command help-prefix-map		:which-key "Help")
    "b"  '(:prefix-command buffer-prefix-map		:which-key "Buffer")
    "e"  '(:prefix-command elisp-normal-prefix-map	:which-key "Elisp")
    "c"  '(:prefix-command code-prefix-map		:which-key "Code")
    "l"  '(:prefix-command latex-prefix-map		:which-key "LaTeX")
    "t"  '(:prefix-command toggle-prefix-map		:which-key "Toggle")
    "o"  '(:prefix-command org-prefix-map		:which-key "Org")
    )
  (define-normal-key
    :prefix "SPC l"
    :prefix-command 'latex-prefix-map
    "c" '((lambda ()
	    (interactive)
	    (save-buffer)
	    (TeX-save-document (TeX-master-file))
	    (TeX-command TeX-command-default
			 'TeX-master-file
			 -1))
	  :which-key "Compile")
    "v" '((lambda ()
	    (interactive)
	    (TeX-command "View" 'TeX-master-file -1))
	  :which-key "View")
    )
  (define-normal-key
    :prefix "SPC o"
    :prefix-command 'org-prefix-map
    "e" '((lambda () (interactive)
            (if (use-region-p)
              (org-babel-execute-region)
              (org-babel-execute-src-block-maybe))) :which-key "Block or region")
    "E" '(org-babel-execute-buffer :which-key "Buffer")
    "b" '(org-next-block :which-key "Next block")
    "B" '(org-previous-block :which-key "Previous block")
    "s" '(org-edit-src-code :which-key "Source")
    "L" '(org-edit-latex-fragment :which-key "Edit LaTeX")
    "i" '(org-insert-structure-template :which-key "Insert")
    )
  (define-normal-key
    :prefix "SPC t"
    :prefix-command 'toggle-prefix-map
    "c" '((lambda () (interactive)
	    (require 'copilot)
	    (copilot-mode)
	    ) :which-key "Copilot")
    "l" '(display-line-numbers-mode :which-key "Line numbers")
    "s" '(tramp-revert-buffer-with-sudo :which-key "Sudo")
    )
  (define-normal-key
    :prefix "SPC c"
    :prefix-command 'code-prefix-map
    "s" '(helm-lsp-workspace-symbol	:which-key "Symbols")
    "d" '(helm-lsp-diagnostics		:which-key "Diagnostics")
    "x" '(xref-find-references-at-mouse :which-key "References at mouse")
    "X" '(xref-find-references		:which-key "References")
    "n" '(+insert-section		:which-key "New section")
    )
  (define-normal-key
    :prefix "SPC h"
    :prefix-command 'help-prefix-map
    "l" '(helm-man-woman		:which-key "Manpages")
    "h" '(helm-apropos			:which-key "Apropos")
    "p" '(helm-packages			:which-key "Packages")
    "b" '(helm-descbinds		:which-key "Binds")
    "m" '(describe-mode				:which-key "Mode")
    "f" '(describe-face				:which-key "Face")
    "c" '(list-colors-display		:which-key "Colors")
    "r" '(helm-register			:which-key "Registers")
    "R" '(restart-emacs			:which-key "Restart")
    "s" '(+describe-foo-at-point		:which-key "Symbol")
    "e" '(view-echo-area-messages	:which-key "Messages")
    "a" '(+theme-set-frame-alpha	:which-key "Alpha")
    )
  (define-normal-key
    :prefix "SPC p"
    :prefix-command 'project-prefix-map
    "p" '(projectile-switch-project :which-key "Switch")
    "a" '(projectile-add-known-project :which-key "Add")
    )
  (define-normal-key
    :prefix "SPC b"
    :prefix-command 'buffer-prefix-map
    "k" '(kill-current-buffer :which-key "Kill")
    )
  (define-normal-key
    :prefix "SPC e"
    :prefix-command 'elisp-normal-prefix-map
    "s" '((lambda () (interactive)
            (select-window (split-window-below))
            (scratch-buffer)
            ) :which-key "Scratch")
    "b" '(eval-buffer :which-key "Eval buffer")
    "i" '((lambda () (interactive)
            (let ((f (function-called-at-point)))
              (when f (edebug-instrument-function f)))
            ):which-key "Instrument")
    "u" '((lambda () (interactive)
            (let ((f (function-called-at-point)))
              (when f (edebug-remove-instrumentation (list f))))
            ):which-key "Uninstrument")
    )

  (define-visual-key
    :prefix "SPC"
    "a" '(align-regexp                             :which-key "Align")
    "e"  '(:prefix-command elisp-visual-prefix-map :which-key "Elisp")
    )
  (define-visual-key
    :prefix "SPC e"
    :prefix-command 'elisp-visual-prefix-map
    "r" '(eval-region :which-key "Eval region")
    ))

(global-set-key (kbd "<escape>") #'keyboard-escape-quit)

(provide 'keybindings)
