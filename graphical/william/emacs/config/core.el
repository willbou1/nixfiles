;; -*- lexical-binding: t; -*-

(require 'use-package)
(require 'lib)
(require 'theme)
(require 'keybindings)
(require 'menu)

(setq help-window-select nil
      user-full-name "William Boulanger")

;; -------------------------------- Line numbers -------------------------------
(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'LaTeX-mode-hook 'display-line-numbers-mode)
(add-hook 'dired-mode-hook 'display-line-numbers-mode)
(add-hook 'org-mode-hook 'display-line-numbers-mode)

(use-package 
  smartparens
  :config
  (setq sp-hybrid-kill-excessive-wihtespace t)
  (smartparens-global-mode))

(use-package
  evil-mc
  :after evil
  :config
  (global-evil-mc-mode 1))

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
(setq make-backup-files t
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
  (dashboard-setup-startup-hook))

(use-package helpful
  :config
  (setq helm-describe-function-function #'helpful-function
        helm-describe-variable-function #'helpful-variable))

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
    (setq-local completion-at-point-functions
		(append
		 (list (cape-capf-super
			#'cape-dabbrev
			#'cape-file
			#'cape-elisp-block
			#'cape-dict))
		 completion-at-point-functions)))
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

;; ---------------------------------- Snippets ---------------------------------
(use-package
  yasnippet
  :config
  (yas-global-mode 1))

;; ------------------------------------ Org ------------------------------------
(setq org-directory "~/priv/documents/org/"
      org-confirm-babel-evaluate nil)

(use-package
  org-fragtog
  :init
  :hook (org-mode-hook . org-fragtog-mode)
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
  :config
  (setq jupyter-eval-use-overlays t)
  (with-eval-after-load 'org-babel
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (python . t)
       (jupyter . t))))
  (add-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images))

;; ------------------------------------ LSP ------------------------------------
(use-package
  lsp-mode
  :hook ((c-mode-hook . lsp-deferred)
	 (c++-mode-hook . lsp-deferred)
	 (haskell-mode-hook . lsp-deferred)
	 (rust-mode-hook . lsp-deferred)))

(use-package
  lsp-ui
  :after lsp-mode
  :hook (lsp-mode-hook . lsp-ui-mode))

;; ------------------------------------ DAP ------------------------------------
(use-package
  dap-lldb
  :hook (c++-mode-hook . (lambda () (require 'dap-lldb)))
  :init
  (setq dap-auto-configure-features '(sessions locals controls tooltip))
  :config
  (setq dap-lldb-debug-program '("lldb-dap")))

;;(helm-lsp-workspace-symbol t)

(require 'feebleline)
(setq feebleline-timer-interval 0.5)
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

(provide 'core)
