;; -*- lexical-binding: t; -*-

(require 'use-package)

;; Evil
(use-package
  undo-tree
  :init
  (make-directory "~/.config/emacs/undotree/" t)
  (add-to-list 'display-buffer-alist '("*undo-tree*" (display-buffer-in-side-window) (side . right)  (window-width . 25)))
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


;; General
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

  (general-create-definer define-everywhere
    :keymaps 'override)

  (general-define-key
   :states '(normal motion)
   :keymaps 'special-mode-map
   "<escape>" #'quit-window)

  (general-define-key
    :states 'normal
    :keymaps 'dashboard-mode-map
    "r" #'+dashboard-jump-to-recents
    "p" #'+dashboard-jump-to-projects)

  (with-eval-after-load 'dired
    (general-define-key
     :states 'normal
     :keymaps 'dired-mode-map
     "a" #'dired-find-file
     "f" #'dired-create-empty-file
     "RET" (lambda () (interactive)
	     (if (f-directory? (dired-get-file-for-visit))
		 (dired-find-alternate-file)
	       (dired-find-file)))))


;; Misc
  (define-everywhere
    "C-S-c" '(clipboard-kill-ring-save				:which-key "Copy")
    "C-S-v" '(clipboard-yank					:which-key "Paste")

    "C-n" #'evil-mc-make-and-goto-next-match
    "C-p" #'evil-mc-make-and-goto-prev-match
    "C-a" #'evil-mc-make-all-cursors
    "C-c" #'evil-mc-make-cursor-here
    "C-x" #'evil-mc-undo-all-cursors

    "C-h" #'sp-kill-hybrid-sexp
    "C-k" #'sp-kill-sexp
    "C-w" (lambda (pair) (interactive "cPair: ")
	    (sp-wrap-with-pair (char-to-string pair)))
    "C-S-w" #'sp-rewrap-sexp
    "C-f" #'sp-unwrap-sexp
    )

  (defun my-indent-after-paste (beg end)
    "Auto-indent region after pasting."
    (when (and (derived-mode-p 'prog-mode 'text-mode) ; adjust modes as needed
               (not (region-active-p)))
      (indent-region beg end)))

  (advice-add 'yank :after
              (lambda (&rest args)
		(my-indent-after-paste (mark t) (point))))

  (defmacro +hydra-custom (name desc &rest opts)
    "Take a description of the form ([OPTIONS] DOC HEADS...) and generate a Hydra for it."
    (unless (symbolp desc)
      (error "+hydra-custom: DESC must be a symbol"))
    (unless (boundp desc)
      (error "+hydra-custom: %s is unbound" desc))
    (let* ((desc-value (symbol-value desc))
	   (fst-value (car desc-value))
	   (additional-opts (if (listp fst-value) fst-value))
	   (doc-and-heads (if (listp fst-value) (cdr desc-value) desc-value)))
      `(defhydra ,name (:hint nil ,@opts ,@additional-opts)
	 ,@doc-and-heads)))

  (defhydra +hydra-helm (:hint nil)
    ("m" helm-toggle-visible-mark "mark")
    ("a" helm-toggle-all-marks "(un)mark all")
    ("v" helm-execute-persistent-action)
    ("g" helm-beginning-of-buffer "top")
    ("h" helm-previous-source)
    ("l" helm-next-source)
    ("G" helm-end-of-buffer "bottom")
    ("j" helm-next-line "down")
    ("k" helm-previous-line "up")
    ("q" keyboard-escape-quit "exit")
    ("<escape>" keyboard-escape-quit "exit")
    ("i" nil "cancel"))

  (general-define-key
   :keymaps 'helm-map
   "S-<return>" #'+hydra-helm/body)


;; Org
  (defconst +hydra-org-table--desc
    '((:foreign-keys run)
      ""
      ("n" org-table-create)
      ("J" org-table-move-row-down)
      ("K" org-table-move-row-up)
      ("l" org-table-next-field)
      ("h" org-table-previous-field)
      ("L" org-table-move-column-right)
      ("H" org-table-move-column-left)
      ("r" org-table-insert-row)
      ("c" org-table-insert-column)
      ("C" org-table-delete-column)
      ("b" org-table-blank-field)
      ("e" org-table-edit-field)
      ("v" org-table-copy-down)
      ("a" org-table-align)
      ("m" org-table-recalculate) ; Give us the Mathhhh
      ("s" org-table-sort-lines)
      ("d" org-table-kill-row)
      ("o" org-table-toggle-coordinate-overlays)
      ("<escape>" nil)
      ("q" nil)))
  (+hydra-custom +hydra-org-table +hydra-org-table--desc)
  (+hydra-custom +hydra-org-table-oneshot +hydra-org-table--desc :exit t)

  (defconst +hydra-org-insert--desc
    '(( :foreign-keys run)
      ""
      ("d" org-insert-drawer)
      ("b" (org-insert-structure-template "src"))
      ("B" org-insert-structure-template)
      ("l" org-insert-link)
      ("h" org-insert-heading)
      ("H" org-insert-heading-respect-content)
      ("S" org-insert-subheading)
      ("i" org-insert-item)
      ("t" org-insert-todo-heading)
      ("T" org-insert-todo-subheading)
      (":" org-insert-time-stamp)
      ("<escape>" nil)
      ("q" nil)))
  (+hydra-custom +hydra-org-insert +hydra-org-insert--desc)
  (+hydra-custom +hydra-org-insert-oneshot +hydra-org-insert--desc :exit t)

  (defconst +hydra-org--desc
    '(( :foreign-keys run)
      "
  _h_: ↑ heading   _l_: ↑ item  _b_: ↑ block   _<_: Demote    _m_: Mark       _i_: Insert   _x_: Execute   _u_: Undo     _n_: Narrow
  _H_: ↓ heading   _L_: ↓ item  _B_: ↓ block   _>_: Promote   _M_: Mark sub   _d_: Delete   _e_: Edit      _r_: Refile   _w_: Widen
  "
      ("h" org-next-visible-heading)
      ("H" org-previous-visible-heading)
      ("l" org-next-item)
      ("L" org-previous-item)
      ("l" org-next-link)
      ("L" org-previous-link)
      ("b" org-next-block)
      ("B" org-previous-block)
      (">" org-promote-subtree)
      ("<" org-demote-subtree)
      ("m" (if (org-in-src-block-p)
	       (org-babel-mark-block)
	     (org-mark-element)))
      ("M" org-mark-subtree)
      ("i" +hydra-org-insert-oneshot/body)
      ("I" +hydra-org-insert/body)
      ("d" (if (use-region-p)
	       (call-interactively #'evil-delete)
	     (org-cut-subtree)))
      ("n" org-narrow-to-element)
      ("w" widen)
      ("u" evil-undo)
      ("c" org-toggle-checkbox)
      ("t" +hydra-org-table-oneshot/body)
      ("T" +hydra-org-table/body)
      ("r" org-refile)
      ("p" org-set-property)
      ("x" (if (use-region-p)
              (org-babel-execute-region)
              (org-babel-execute-src-block-maybe)))
      ("X" org-babel-execute-buffer)
      ("e" org-edit-special :exit t)
      ("<escape>" nil)
      ("q" nil)))
  (+hydra-custom +hydra-org +hydra-org--desc)
  (+hydra-custom +hydra-org-oneshot +hydra-org--desc :exit t)


;; Windows
  (defun +hydra-window--desc-base (select-new-window)
    (let ((speed 3)
	  (split (lambda (direction)
		   (lambda () (interactive)
		     (let ((new-window (split-window (selected-window) nil direction)))
		       (when select-new-window
			 (select-window new-window)))))))
        `("
_h_: ←   _j_: ↓   _H_: w += 3   _J_: h += 3   _v_: ||   _r_: ⟳   _e_: Exchange   _m_: Maximize   _d_: Delete   _t_: topdown     _u_: Undo   _q_: Quit
_l_: →   _k_: ↑   _L_: w -= 3   _K_: h -= 3   _s_: ==   _R_: ⟲   _i_: Isolate    _b_: Balance    _w_: Ace      _f_: leftright   _U_: Redo
"
          ("<left>" (evil-window-left 1))
          ("<right>" (evil-window-right 1))
          ("<down>" (evil-window-down 1))
          ("<up>" (evil-window-up 1))

          ("h" (evil-window-left 1))
          ("l" (evil-window-right 1))
          ("j" (evil-window-down 1))
          ("k" (evil-window-up 1))

          ("H" (evil-window-increase-width ,speed))
          ("L" (evil-window-decrease-width ,speed))
          ("J" (evil-window-increase-height ,speed))
          ("K" (evil-window-decrease-height ,speed))

          ("v" ,(funcall split 'right))
          ("V" ,(funcall split 'left))
          ("s" ,(funcall split 'below))
          ("S" ,(funcall split 'above))
          ("e" evil-window-exchange)
          ("i" delete-other-windows)
          ("m" maximize-window)
          ("b" balance-windows)
          ("d" evil-window-delete)
	  ("t" window-layout-flip-topdown)
	  ("f" window-layout-flip-leftright)
          ("r" window-layout-rotate-clockwise)
          ("R" window-layout-rotate-anticlockwise)
	  ("=" (text-scale-adjust 1))
	  ("+" (text-scale-adjust 1))
	  ("-" (text-scale-adjust -1))
	  ("p" (kill-buffer-and-window))
	  ("u" winner-undo)
	  ("U" winner-redo)
          ("W" (let ((aw-scope 'frame))
		 (ace-window)))
          ("w" ace-window)
	  ("<tab>" aw-flip-window)
	  ("<escape>" nil)
          ("q" nil))))
  (defconst +hydra-window--desc (+hydra-window--desc-base nil))
  (defconst +hydra-window--desc-oneshot (+hydra-window--desc-base t))
  (+hydra-custom +hydra-window +hydra-window--desc)
  (+hydra-custom +hydra-window-oneshot +hydra-window--desc-oneshot :exit t)


  ;; Root
  (define-normal-key
    "<backspace>" #'evil-jump-backward
    "S-<backspace>" #'evil-jump-forward
    "C-<backspace>" #'evil-visual-restore
    )
  (define-normal-key
    :prefix "SPC"
    "SPC" '(helm-M-x						:which-key "M-x")
    ";" '(eval-expression					:which-key "Eval")
    "," '(helm-buffers-list					:which-key "Find buffer")
    "." '(helm-projectile					:which-key "Projectile")
    ":" '(async-shell-command					:which-key "Async $")
    "é" '(helm-for-files					:which-key "Find file")
    "g" '(magit							:which-key "Magit")
    "$" '((lambda ()
	    (interactive)
	    (let ((process-environment initial-environment))
	      (start-process "kitty" nil "kitty" "-d" default-directory)))(
	    )							:which-key "Terminal")
    "|" '((lambda (command) (interactive "sShell command: ")
	    (if (use-region-p)
		(shell-command-on-region (region-beginning) (region-end) command nil t)
	      (shell-command-on-region (point-min) (point-max) command nil t)
	      ))						:which-key "Process")
    "n" '((lambda () (interactive)
	    (forward-page)
	    (forward-char)) :which-key "Next page")
    "N" '((lambda () (interactive)
	    (backward-page)
	    (forward-char)) :which-key "Prev page")
    "f" '(helm-do-grep-ag					:which-key "Find")
    "u" '(undo-tree-visualize :which-key "Undo tree")
    "s" '(helm-yas-complete					:which-key "Snippet")
    "r" '(helm-tramp						:which-key "Tramp")
    "d" '(dired							:which-key "Dired")
    "w" '(+hydra-window-oneshot/body				:which-key "Window oneshot")
    "i" '(ement-room-list :which-key "Matrix")
    "W" '(+hydra-window/body					:which-key "Window")
    "o" '(+hydra-org-oneshot/body				:which-key "Org oneshot")
    "O" '(+hydra-org/body					:which-key "Org")
    "k" '(helm-epa-list-keys					:which-key "Keys")
    "a" '(gptel :which-key "AI")
    "=" '((lambda () (interactive)
	    (global-text-scale-adjust 1)) :which-key "Zoom in")
    "+" '((lambda () (interactive)
	    (global-text-scale-adjust 1)) :which-key "Zoom in")
    "-" '((lambda () (interactive)
	    (global-text-scale-adjust -1)) :which-key "Zoom out")
    "<backspace>" '(dashboard-open				:which-key "Dashboard")
    "TAB" '(aw-flip-window						:which-key "Other window")
    "m" '(helm-all-mark-rings :which-key "Mark ring")
    "RET" '((lambda () (interactive)
	      (switch-to-buffer (other-buffer)))		:which-key "Other buffer")
    "p"  '(:prefix-command project-prefix-map			:which-key "Project")
    "h"  '(:prefix-command help-prefix-map			:which-key "Help")
    "b"  '(:prefix-command buffer-prefix-map			:which-key "Buffer")
    "e"  '(:prefix-command elisp-prefix-map			:which-key "Elisp")
    "c"  '(:prefix-command code-prefix-map			:which-key "Code")
    "l"  '(:prefix-command latex-prefix-map			:which-key "LaTeX")
    "t"  '(:prefix-command toggle-prefix-map			:which-key "Toggle")
    "f"  '(:prefix-command file-prefix-map			:which-key "File")
    )

;; LaTeX
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
    :prefix "SPC f"
    :prefix-command 'file-prefix-map
    "e" '(epa-encrypt-file					:which-key "Encrypt with PGP")
    "s" '(sops-edit-file :which-key "Edit with sops")
    "S" '(sops-save-file :which-key "Save with sops")
    )

;; Toggle
  (define-normal-key
    :prefix "SPC t"
    :prefix-command 'toggle-prefix-map
    "c" '((lambda () (interactive)
	    (require 'copilot)
	    (copilot-mode)
	    )							:which-key "Copilot")
    "o" '(olivetti-mode						:which-key "Olivetti")
    "t" '(toggle-truncate-lines :which-key "Truncate lines")
    "w" '(toggle-word-wrap :which-key "Word wrap")
    "l" '(display-line-numbers-mode				:which-key "Line numbers")
    "s" '(tramp-revert-buffer-with-sudo				:which-key "Sudo")
    "d" '(rainbow-delimiters-mode				:which-key "Rainbow delimiters")
    "i" '(rainbow-identifiers-mode				:which-key "Rainbow identifiers")
    "m" '(minimap-mode						:which-key "Minimap")
    "|" '(display-fill-column-indicator-mode			:which-key "Fill column")
    "p" '(smartparens-mode					:which-key "Smartparens")
    "c" '(hl-line-mode :which-key "Hl line")
    "h" '(lsp-toggle-symbol-highlight :which-key "Hl symbols")
    )

;; Code
  (define-normal-key
    :prefix "SPC c"
    :prefix-command 'code-prefix-map
    "s" '(helm-lsp-workspace-symbol				:which-key "Symbols")
    "c" '(helm-lsp-diagnostics					:which-key "Diagnostics")
    "x" '(xref-find-references-at-mouse				:which-key "References at mouse")
    "X" '(xref-find-references					:which-key "References")
    "n" '(+insert-section					:which-key "New section")
    "i" '(lsp-ui-imenu :which-key "IMenu")
    "d" '(lsp-ui-doc-glance :which-key "Doc")
    "D" '(lsp-ui-doc-toggle :which-key "Doc")
    )

  ;;Help
  (define-normal-key
    :prefix "SPC h"
    :prefix-command 'help-prefix-map
    "t" '(helm-timers						:which-key "Timers")
    "l" '(helm-man-woman					:which-key "Manpages")
    "h" '(helm-apropos						:which-key "Apropos")
    "p" '(helm-packages						:which-key "Packages")
    "b" '(helm-descbinds					:which-key "Binds")
    "m" '(describe-mode						:which-key "Mode")
    "f" '(describe-face						:which-key "Face")
    "c" '(list-colors-display					:which-key "Colors")
    "r" '(helm-register						:which-key "Registers")
    "R" '(restart-emacs						:which-key "Restart")
    "e" '(view-echo-area-messages				:which-key "Messages")
    "a" '(+theme-set-frame-alpha				:which-key "Alpha")
    "s" '(helpful-at-point		:which-key "Symbol")
    "w" '(dictionary-search :which-key "Word")
    "N" '((lambda () (interactive)
	    (dired "/etc/nixos")
	    )							:which-key "Nixos config")
    "E" '((lambda () (interactive)
	    (dired "/etc/nixos/graphical/william/emacs")
	    )							:which-key "Emacs config")
    )

;; Project
  (define-normal-key
    :prefix "SPC p"
    :prefix-command 'project-prefix-map
    "p" '(projectile-switch-project				:which-key "Switch")
    "a" '(projectile-add-known-project				:which-key "Add")
    "f" '(helm-projectile-rg :which-key "Find in")
    )
  (define-normal-key
    :prefix "SPC b"
    :prefix-command 'buffer-prefix-map
    "k" '(kill-current-buffer					:which-key "Kill")
    "e" '(epa-encrypt-region					:which-key "Encrypt region")
    "b" '((lambda () (interactive)
	    (let ((helm-boring-buffer-regexp-list '()))
	      (helm-buffers-list)))				:which-key "All buffers")
    )

;; ELisp
  (define-normal-key
    :prefix "SPC e"
    :prefix-command 'elisp-prefix-map
    "s" '(scratch-buffer					:which-key "Scratch")
    "e" '(+keybindings-eval					:which-key "Eval")
    "b" '(eval-buffer						:which-key "Eval buffer")
    "m" '(helm-imenu						:which-key "Eval buffer")
    "d" '(eval-defun					:which-key "Eval defun")
    "i" '((lambda () (interactive)
	    (let ((f (function-called-at-point)))
	      (when f (edebug-instrument-function f)))
	    )							:which-key "Instrument")
    "u" '((lambda () (interactive)
	    (let ((f (function-called-at-point)))
	      (when f (edebug-remove-instrumentation (list f))))
	    )							:which-key "Uninstrument")
    "$" '(eshell						:which-key "EShell")
    )

  ;; $$ \cos(\theta) $$
  (defun +keybindings-eval ()
    "This is like eval-defun, but I execute the closest sexp even if inside a defun or defmacro."
    (interactive)
    (if (use-region-p)
	(+eval-region (region-beginning) (region-end))
      (save-excursion
	(when (looking-at-p "(")
	  (evil-next-close-paren))
	(if-let* ((paragraph-bounds (bounds-of-thing-at-point 'paragraph)))
	    (if-let* ((state (syntax-ppss))
		      (start-paren (nth 1 state))
		      (look-ahead (max (point-min) (1- start-paren)))
		      (look-ahead-char (char-after look-ahead)))
		(progn
		  (when (or (eq ?' look-ahead-char) (eq ?` look-ahead-char))
		    (setq start-paren (1- start-paren)))
		  (let* ((end-paren (scan-sexps start-paren 1))
			 (paren-extent (- end-paren start-paren))
			 (paragraph-extent (- (cdr paragraph-bounds) (car paragraph-bounds))))
		    (if (and start-paren (>= paragraph-extent paren-extent))
			(+eval-region start-paren end-paren)
		      (+eval-region (car paragraph-bounds) (cdr paragraph-bounds)))))
	      (+eval-region (car paragraph-bounds) (cdr paragraph-bounds)))
	  (when (y-or-n-p "Eval buffer as ELisp? ")
	    (eval-buffer))))))

  (defun +eval-region (start end)
    (interactive "r")
    (let* ((text (buffer-substring-no-properties start end))
           (forms (read (concat "(progn " text "\n)"))))
      (message "%S" (eval forms))))

  (define-visual-key
    :prefix "SPC"
    "a" '(align-regexp						:which-key "Align")
    ))

;; Fix ESC doing stupid stuff all the time
(global-set-key (kbd "<escape>") #'keyboard-escape-quit)

;; Fix opening org links with RET
(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "RET") nil))

(provide 'keybindings)

;; Local Variables:
;; eval: (add-hook 'before-save-hook
;;                 (lambda ()
;;                       (align-regexp (point-min) (point-max)
;;                                     "\\(\\s-*\\)		:which-key" 1 1 t)))
;; End:
