;; -*- lexical-binding: t; -*-

(require 'use-package)
(require 'lib)
(require 'theme)
(require 'keybindings)
(require 'menu)

(setq help-window-select nil
      aw-ignored-buffers '(" *MINIMAP*")
      minimap-minimum-width 18
      scroll-margin 3
      scroll-conservatively 10
      user-full-name "William Boulanger")

;; -------------------------------- Line numbers -------------------------------
(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'LaTeX-mode-hook 'display-line-numbers-mode)
(add-hook 'dired-mode-hook 'display-line-numbers-mode)
(add-hook 'org-mode-hook 'display-line-numbers-mode)

(use-package smartparens
  :hook ((prog-mode . smartparens-mode)
	 (eval-expression-minibuffer-setup . smartparens-mode))
  :init
  (setq sp-hybrid-kill-excessive-whitespace t)
  :config
  (sp-with-modes '(emacs-lisp-mode minibuffer-mode)
    (sp-local-pair "'" nil :actions nil)
    (sp-local-pair "`" nil :actions nil)))

(use-package
  evil-mc
  :after evil
  :config
  (global-evil-mc-mode 1))

(use-package
  direnv
  :commands direnv-mode
  :hook (find-file . direnv-mode))

;; ----------------------------------- Dired -----------------------------------
(with-eval-after-load 'dired
  (put 'dired-find-alternate-file 'disabled nil))

(use-package 
  dired-subtree
  :after dired)

;; -------------------------------- Fill column --------------------------------
(setq fill-column 80)
(add-hook 'text-mode-hook #'auto-fill-mode)

(setq global-hl-line-sticky-flag t)
(global-hl-line-mode 1)

;; (setq display-fill-column-indicator-column 80)
;; (add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

;; --------------------------- Backups and autosaves ---------------------------
(make-directory "~/.config/emacs/backups/" t)
(setq auto-save-no-message t
      make-backup-files t
      backup-by-copying t
      backup-by-copying-when-linked t
      version-control t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      backup-directory-alist '(("." . "~/.config/emacs/backups/"))
      tramp-allow-unsafe-temporary-files t
      tramp-default-method "ssh")

;; --------------------------------- Dashboard ---------------------------------
(use-package
  page-break-lines
  :config
  (global-page-break-lines-mode))

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
	dashboard-startup-banner "~/.config/fastfetch/images/image1.png"
	dashboard-image-banner-max-height 300
	dashboard-page-separator "\n\f\n"
	dashboard-center-content nil
	dashboard-vertically-center-content t
	dashboard-show-shortcuts t
	dashboard-display-icons-p t
	dashboard-icon-type 'nerd-icons
	dashboard-set-heading-icons t
	dashboard-set-file-icons t
	initial-buffer-choice 'dashboard-open)
  (set-face-underline 'dashboard-items-face nil)
  (set-face-underline 'dashboard-no-items-face nil)
  (add-hook 'server-after-make-frame-hook 'dashboard-open)

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

  (add-hook 'dashboard-mode-hook '+dashboard-jump-to-recents)
  (add-hook 'dashboard-mode-hook 'page-break-lines-mode)
  (dashboard-setup-startup-hook))

(use-package helpful
  :config
  (setq helpful-max-buffers 2)
  (with-eval-after-load 'helm
    (setq helm-describe-function-function #'helpful-function
	  helm-describe-variable-function #'helpful-variable)))

;; --------------------------------- Clipboard ---------------------------------
(setq select-enable-clipboard nil)
(setq select-enable-primary nil)

;; --------------------------------- Completion --------------------------------
(use-package
  corfu
  :config
  (setq tab-always-indent 'complete
        corfu-auto t
	corfu-preview-current nil
        corfu-auto-delay 0.2
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

;; ---------------------------------- Projects ---------------------------------
(use-package
  projectile
  :config
  (setq projectile-project-search-path '("~/priv/code"
					 "~/priv/documents")
        projectile-completion-system 'helm
        projectile-enable-caching t
        projectile-indexing-method 'hybrid)
  (projectile-mode 1))

;; Symbols
(setq prettify-symbols-unprettify-at-point t)
(add-hook 'prog-mode-hook #'prettify-symbols-mode)

;; ---------------------------------- Treesit ----------------------------------
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

;; ---------------------------------- Snippets ---------------------------------
(use-package
  yasnippet
  :config
  (yas-global-mode 1))

;; ------------------------------------ Org ------------------------------------
(use-package
  org
  :config
  (setq org-directory "~/priv/documents/org/"
	org-confirm-babel-evaluate nil
	org-return-follows-link t))

(use-package
  org-appear
  :hook (org-mode . org-appear-mode))

(use-package
  toc-org
  :hook (org-mode . toc-org-mode))

(use-package
  org-fragtog
  :init
  :hook (org-mode . org-fragtog-mode)
  :config
  (plist-put org-format-latex-options :scale 2.0)
  (plist-put org-format-latex-options :background "Transparent"))

(use-package
  org-modern
  :after org
  :config
  (setq rg-auto-align-tags nil
	org-tags-column 0
	org-catch-invisible-edits 'show-and-error
	org-special-ctrl-a/e t
	org-insert-heading-respect-content t

	org-hide-emphasis-markers t
	org-pretty-entities t
	org-agenda-tags-column 0
	org-ellipsis "…")
  (global-org-modern-mode))

;; ----------------------------------- LaTeX -----------------------------------
(with-eval-after-load 'tex
  (setq TeX-PDF-mode t
	TeX-auto-save t
        TeX-source-correlate-mode t
        TeX-source-correlate-start-server nil
	org-preview-latex-default-process 'dvisvgm
	org-latex-packages-alist
	'(("" "tikz" t)
	  ("" "tikz-cd" t)
	  ("" "circuitikz" t)
	  ("" "pgfplots" t))
	)
  (setcar (cdr (assoc 'output-pdf TeX-view-program-selection))
	  "Zathura")
  (plist-put org-format-latex-options :scale 1.7)
  (add-hook 'LaTeX-mode-hook (lambda ()
                               (require 'tex-fold)
                               (TeX-fold-mode 1)
			       (TeX-fold-buffer)

                               (require 'evil-tex)
                               (evil-tex-mode))))

;; ---------------------------------- Jupyter ----------------------------------
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

;; ------------------------------------ LSP ------------------------------------
(use-package
  lsp-mode
  :hook ((c-mode . lsp-deferred)
	 (c++-mode . lsp-deferred)
	 (haskell-mode . lsp-deferred)
	 (rust-mode . lsp-deferred)))

(use-package
  lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode))

;; ------------------------------------ DAP ------------------------------------
(use-package
  dap-lldb
  :hook (c++-mode . (lambda () (require 'dap-lldb)))
  :init
  (setq dap-auto-configure-features '(sessions locals controls tooltip))
  :config
  (setq dap-lldb-debug-program '("lldb-dap")))

;;(helm-lsp-workspace-symbol t)

(require 'feebleline)
(setq feebleline-timer-interval 0.1)
(feebleline-mode 1)

;; ----------------------------------- GPTel -----------------------------------
(use-package
  gptel
  :config
  (setq gptel-model  'gpt-4o
      gptel-backend
      (gptel-make-openai "Github Models" ;Any name you want
        :host "models.inference.ai.azure.com"
        :endpoint "/chat/completions?api-version=2024-05-01-preview"
        :stream t
        :key ""
        :models '(gpt-4o gpt-5))))

(use-package
  ai-images
  :init
  (setq ai-images-default-width  800
        ai-images-default-height 800
        ai-images-average-width  200
        ai-images-together-api-key sops--ai-images-api-key))

(provide 'core)
