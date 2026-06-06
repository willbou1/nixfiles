;; -*- lexical-binding: t; -*-

(defun +dashboard-open ()
  (interactive)
  (dashboard-open)
  (run-with-idle-timer 0.1 nil
		       (lambda ()
			 (+dashboard-jump-to-recents)
			 (page-break-lines-mode +1)
			 (hl-line-mode +1))))

(require 'use-package)
(require 'lib)
(require 'theme)
(require 'keybindings)
(require 'menu)
(require 'lang)

(setq help-window-select nil
      split-width-threshold 130
      aw-ignored-buffers '(" *MINIMAP*")

      minimap-minimum-width 18
      minimap-window-location 'right
      minimap-hide-fringes t
      minimap-major-modes '(prog-mode org-mode)

      scroll-margin 3
      scroll-conservatively 10
      require-final-newline t
      user-full-name "William Boulanger")

(winner-mode 1)

;; Fix for bad mm on UPERFECT displays
(when (-contains? tramp-local-host-names "haskellslayslay")
  (setq display-mm-dimensions-alist
	'((t . (399 . 224)))))


;; Line numbers
(setq display-line-numbers-type 'relative)
(dolist (hook '(prog-mode-hook LaTeX-mode-hook dired-mode-hook org-mode-hook))
  (add-hook hook #'display-line-numbers-mode))

(use-package smartparens
  :hook ((prog-mode . smartparens-mode)
	 (eval-expression-minibuffer-setup . smartparens-mode))
  :init
  (setq sp-hybrid-kill-excessive-whitespace t)
  :config
  (add-to-list 'sp-ignore-modes-list 'org-mode)
  (sp-with-modes '(emacs-lisp-mode minibuffer-mode)
    (sp-local-pair "'" nil :actions nil)
    (sp-local-pair "`" nil :actions nil))
  (add-hook 'eval-expression-minibuffer-setup-hook #'smartparens-mode)
  (add-to-list 'sp-ignore-modes-list 'org-mode)
  (smartparens-global-mode))

(use-package
  evil-mc
  :after evil
  :config
  (global-evil-mc-mode 1))

(use-package
  direnv
  :commands direnv-mode
  :hook ((after-init . direnv-mode))
	 ;;(lsp-before-initialize-hook . direnv-update-environment))
  :init
  (setq direnv-always-show-summary nil))


;; Dired
(with-eval-after-load 'dired
  (put 'dired-find-alternate-file 'disabled nil))

(use-package 
  dired-subtree
  :after dired)


;; Fill column, truncate and word wrap
(setq fill-column 80)
(add-hook 'text-mode-hook #'auto-fill-mode)

(global-visual-line-mode 1)
(setq-default truncate-lines nil
	      truncate-partial-width-windows nil
	      word-wrap t)

;; (setq display-fill-column-indicator-column 80)
;; (add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

(setq global-hl-line-sticky-flag t)
(global-hl-line-mode 1)


;; Backups and autosaves
(make-directory "~/.config/emacs/backups/" t)
(make-directory "~/.config/emacs/auto-save/" t)
(setq auto-save-no-message t
      auto-save-file-name-transforms
	`((".*" "~/.config/emacs/auto-save/" t))

      version-control t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      make-backup-files t
      backup-directory-alist '(("." . "~/.config/emacs/backups/"))
      backup-by-copying t
      backup-by-copying-when-linked t)


;; Backups and autosaves
(setq tramp-allow-unsafe-temporary-files t
      tramp-default-method "ssh")


;; Ace window
(defun +kill-buffer-and-window (window)
  "Kill WINDOW and its associated buffer."
  (with-selected-window window
    (kill-buffer-and-window)))
(use-package
  ace-window
  :config
  (setq aw-ignore-current nil
	aw-minibuffer-flag t
	aw-dispatch-always t
	aw-ignored-buffers '(" *MINIMAP*")
	aw-keys '(?a ?r ?t ?n ?i ?o ?g ?h)
	aw-dispatch-alist '((?d aw-delete-window "Delete window")
			    (?m maximize-window "Maximize window")
			    (?e aw-swap-window "Swap windows")
			    (?c aw-copy-window "Copy window")
			    (?p +kill-buffer-and-window "Pulverize")
			    (?s aw-split-window-vert "Split window")
			    (?v aw-split-window-horz "VSplit window"))))


;; Dashboard
(use-package
  page-break-lines
  :config
  (add-to-list 'page-break-lines-modes 'LaTeX-mode)
  (global-page-break-lines-mode 1))

(use-package
  dashboard
  :after page-break-lines
  :config
  (setq dashboard-items '((recents   . 5)
			  (projects  . 5)
			  (bookmarks . 5)
			  (agenda    . 10))
	dashboard-item-shortcuts '((recents   . "r")
				   (projects  . "p")
				   (bookmarks . "b")
				   (agenda    . "a"))
	dashboard-banner-logo-title "Let there be math!"
	dashboard-startup-banner "~/.config/emacs/splash/gamma.png"
	dashboard-projects-backend 'projectile
	dashboard-image-banner-max-height 260
	dashboard-page-separator "\n\f\n"
	dashboard-center-content nil
	dashboard-vertically-center-content t
	dashboard-show-shortcuts t
	dashboard-display-icons-p t
	dashboard-icon-type 'nerd-icons
	dashboard-set-heading-icons t
	dashboard-set-file-icons t
	dashboard-projects-switch-function 'projectile-switch-project-by-name
	dashboard-filter-agenda-entry #'dashboard-filter-agenda-by-todo
	initial-buffer-choice 'dashboard-open)
  (set-face-underline 'dashboard-items-face nil)
  (set-face-underline 'dashboard-no-items-face nil)

  (defun +dashboard-jump-to-recents ()
    (interactive)
    (if (fboundp 'dashboard-jump-to-recents)
      (dashboard-jump-to-recents)
      (message "No recent files available.")))

  (defun +dashboard-jump-to-projects ()
    (interactive)
    (if (fboundp 'dashboard-jump-to-projects)
      (dashboard-jump-to-projects)
      (message "No projects available.")))

  (add-hook 'server-after-make-frame-hook #'+dashboard-open)
  (add-hook 'dashboard-after-initialize-hook #'+dashboard-open)
  (dashboard-setup-startup-hook))

(use-package helpful
  :config
  (setq helpful-max-buffers 2)
  (with-eval-after-load 'helm
    (setq helm-describe-function-function #'helpful-function
	  helm-describe-variable-function #'helpful-variable)))


;; Clipboard
(setq select-enable-clipboard nil)
(setq select-enable-primary nil)


;; Magit
(use-package
  magit
  :init
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  )


;; Completion
(use-package
  corfu
  :config
  (setq tab-always-indent 'complete
        corfu-auto t
	corfu-preview-current nil
        corfu-auto-delay 0.3
        corfu-auto-trigger "."
	corfu-popupinfo-delay 0.7
        corfu-quit-no-match 'separator
	dabbrev-check-all-buffers nil)
  (defun set-up-completions ()
    "Add CAPE sources on top of existing completion-at-point-functions."
    (require 'cape)
    (defvar-local orig-capfs nil)
    (unless orig-capfs
      (setq-local orig-capfs completion-at-point-functions))
    (setq-local completion-at-point-functions
		(mapcar
		 (lambda (f)
		   (cond
                    ((functionp f) (cape-capf-super f :with #'cape-dabbrev #'cape-file))
                    ((eq f t)     (cape-capf-super #'cape-dabbrev #'cape-file))
                    (t f)))  ; keep nil or other entries as-is
		 orig-capfs)))
  (add-hook 'prog-mode-hook #'set-up-completions)
  (add-hook 'org-mode-hook #'set-up-completions)
  (add-hook 'emacs-lisp-mode-hook #'set-up-completions)
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  (corfu-history-mode))

(use-package
  orderless
  :config
  (setq completion-styles '(orderless basic)
        completion-category-overrides '((file (styles partial-completion)))
        completion-pcm-leading-wildcard t
	orderless-component-separator " +\\|[/]")
  (add-to-list 'orderless-matching-styles 'char-fold-to-regexp))


;; Projects
(use-package
  projectile
  :config
  (setq projectile-project-search-path '("~/priv/code"
					 "~/priv/documents"
					 "~/priv/nextcloud")
        projectile-completion-system 'helm
        projectile-enable-caching t
        projectile-indexing-method 'hybrid)
  (projectile-mode 1))

;; Symbols
(setq prettify-symbols-unprettify-at-point t)
(add-hook 'prog-mode-hook #'prettify-symbols-mode)


;; Treesit
(let ((languages '(css
		   html
		   (javascript :mode js)
		   json
		   (typescript :dir "typescript/src")
		   (tsx :repo "typescript" :dir "tsx/src")
		   c
		   (cpp :mode c++)
		   (glsl :owner "tree-sitter-grammars")
		   rust go
		   bash
		   python
		   ;; TODO (nix :owner "nix-community") reenable once there is a ts mode
		   (lua :owner "tjdevries")
		   (markdown :owner "ikatyang")
		   (yaml :owner "ikatyang")
		   toml)))
  (setq treesit-language-source-alist
	(--map (if (consp it)
		   (cl-destructuring-bind (n &key repo dir owner mode) it
		     (let ((owner (or owner "tree-sitter"))
			   (repo (or repo (symbol-name n))))
		       `(,n . (,(concat "https://github.com/" owner
					"/tree-sitter-" repo) "master" ,dir))))
		 `(,it . (,(concat "https://github.com/tree-sitter/tree-sitter-"
				   (symbol-name it))))) languages)
	major-mode-remap-alist
	(--map (if (consp it)
		   (cl-destructuring-bind (n &key repo dir owner mode) it
		     (let ((n (symbol-name (or mode n))))
		       (cons (intern (concat n "-mode"))
			     (intern (concat n "-ts-mode")))))
		 (cons (intern (concat (symbol-name it) "-mode"))
		       (intern (concat (symbol-name it) "-ts-mode")))) languages))
  (defun treesit-install-all-language-grammars ()
    "Install all treesitter language grammers at once (When using w/o NixOS)"
    (interactive)
    (mapc (lambda (l) (unless (treesit-language-available-p l)
		   (treesit-install-language-grammar l)))
	  (mapcar #'car treesit-language-source-alist))))


;; Snippets
(use-package
  yasnippet
  :config
  (yas-global-mode 1))


;; Org
(use-package
  org
  :hook (org-mode . follow-mode)
  :init
  (setq org-directory "~/priv/nextcloud/org/"
	org-agenda-files '("~/priv/nextcloud/org/agenda.org")
	browse-url-browser-function 'browse-url-qutebrowser
	browse-url-qutebrowser-arguments '("--target" "tab")
	org-return-follows-link t
	org-make-toc-link-type-fn 'org-make-toc--link-entry-org

	org-src-preserve-indentation t
	org-confirm-babel-evaluate nil

	org-preview-latex-default-process 'dvisvgm
	org-preview-latex-image-directory "~/.config/emacs/ltximg/"
	org-latex-packages-alist '(("" "amsmath" t)
				   ("" "amssymb" t)
				   ("" "tikz" t)
				   ("" "tikz-cd" t)
				   ("" "circuitikz" t)
				   ("" "pgfplots" t))
	org-use-sub-superscripts '{}
	org-startup-with-latex-preview t)

  :config
  (plist-put org-format-latex-options :scale 1.5)
  (plist-put org-format-latex-options :background "Transparent"))

(defun +org-journal-file ()
  (let ((dir (expand-file-name "journal/" org-directory)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (expand-file-name
     (format-time-string "%Y-%m-%d.org")
     dir)))

(use-package
  org-capture
  :after org
  :hook (org-capture-mode . (lambda ()
			      (setq header-line-format nil)))
  :init
  (setq org-capture-templates
	'(("j" "Journal"
	   entry
	   (file (lambda ()
		   (+org-journal-file)))
	   "* %<%H:%M>\n%?"))))

(use-package
  org-appear
  :hook (org-mode . org-appear-mode))

(use-package
  toc-org
  :hook (org-mode . toc-org-mode))

(use-package
  org-fragtog
  :hook (org-mode . org-fragtog-mode))

(use-package
  org-download
  :hook (org-mode . org-download-enable)
  :config ;; We need org stuff to be set up for sure
  (setq org-download-image-dir
	(expand-file-name "images/" org-directory)))

(use-package
  org-modern
  :init
  (setq org-auto-align-tags nil
	org-tags-column 0
	org-catch-invisible-edits 'show-and-error
	org-special-ctrl-a/e t
	org-insert-heading-respect-content t

	org-hide-emphasis-markers t
	org-pretty-entities t
	org-agenda-tags-column 0
	org-ellipsis "…")
  :config
  (global-org-modern-mode))


;; Jupyter
(use-package
  jupyter
  :after (org direnv)
  :init
  (setq jupyter-eval-use-overlays t)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (jupyter . t)))
  (add-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images))


;; mu4e
(use-package
  mu4e
  :init
  (setq mail-user-agent 'mu4e
	mu4e-get-mail-command "mbsync -a"
	mu4e-update-interval 300
	user-mail-address  "willbou2@gmail.com"
	user-full-name     "William Boulanger"
	mu4e-inbox-folder  "/gmail/Inbox"
	mu4e-sent-folder   "/gmail/[Gmail]/Sent Mail"
	mu4e-drafts-folder "/gmail/[Gmail]/Drafts"
	mu4e-trash-folder  "/gmail/[Gmail]/Trash"

	message-send-mail-function 'message-send-mail-with-sendmail
	sendmail-program "msmtp"))


;; GPTel
(use-package
  gptel
  :hook (gptel-mode . gptel-highlight-mode)
  :config
  (setq gptel-model  'gpt-4o
	gptel-default-mode 'org-mode
	gptel-prompt-prefix-alist '((org-mode . "\f\n*Prompt:* "))
	gptel-highlight-methods '(margin fringe)
	gptel-backend
	(gptel-make-openai "Github Models"
	  :host "models.inference.ai.azure.com"
	  :endpoint "/chat/completions?api-version=2024-05-01-preview"
	  :stream t
	  :key sops--github-models
	  :models '(gpt-4o gpt-5))))

(use-package
  ai-images
  :init
  (setq ai-images-default-width  800
        ai-images-default-height 800
        ai-images-average-width  200
        ai-images-together-api-key sops--ai-images-api-key))

(use-package
  sops
  :hook (yaml-ts-mode . sops-mode))

(use-package
  ement
  :init
  (setq ement-sessions-file "~/.config/emacs/ement.el"
	ement-save-sessions t))

(provide 'core)
