;; -*- lexical-binding: t; -*-

(require 'use-package)
(require 'lib)
(require 'theme)
(require 'keybindings)

(setq help-window-select t
      user-full-name "William Boulanger")

;; -------------------------------- Line numbers -------------------------------
(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'LaTeX-mode-hook 'display-line-numbers-mode)
(add-hook 'dired-mode-hook 'display-line-numbers-mode)

;; -------------------------------- Fill column --------------------------------
(setq fill-column 80)
(add-hook 'text-mode-hook #'auto-fill-mode)
(add-hook 'prog-mode-hook #'electric-pair-mode)
(add-hook 'prog-mode-hook #'hl-line-mode)

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
      backup-directory-alist '(("." . "~/.config/emacs/backups/")))

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
	dashboard-center-content t
	dashboard-vertically-center-content t
	dashboard-show-shortcuts t
	dashboard-display-icons-p t
	dashboard-icon-type 'nerd-icons
	dashboard-set-heading-icons t
	dashboard-set-file-icons t
	initial-buffer-choice 'dashboard-open)
  (set-face-underline 'dashboard-items-face nil)
  (set-face-underline 'dashboard-no-items-face nil)
  (add-hook 'dashboard-mode-hook 'dashboard-jump-to-recents)
  (add-hook 'server-after-make-frame-hook 'dashboard-open)
  (general-define-key
   :states 'normal
   :keymaps 'dashboard-mode-map
   "r" #'dashboard-jump-to-recents
   "p" #'dashboard-jump-to-projects)
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
	corfu-popupinfo-delay 0.6
        corfu-quit-no-match 'separator)
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  (corfu-history-mode)
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package
  orderless
  :config
  (setq completion-styles '(orderless basic)
        completion-category-overrides '((file (styles partial-completion)))
        completion-pcm-leading-wildcard t
	orderless-component-separator " +\\|[/]")
  (add-to-list 'orderless-matching-styles 'char-fold-to-regexp))

;; ------------------------------------ Helm -----------------------------------
(use-package
  helm
  :config
  (defun helm-def-source--emacs-variables (&optional default)
    (helm-build-in-buffer-source "Variables"
      :init (lambda ()
	      (helm-apropos-init
	       (lambda (x)
		 (and (boundp x) (not (keywordp x)) (not (class-p x))))
	       default))
      :fuzzy-match helm-apropos-fuzzy-match
      :filtered-candidate-transformer
      (delq nil (list (and (null helm-apropos-fuzzy-match)
			   'helm-apropos-default-sort-fn)
		      (and (null (memq 'helm-apropos helm-commands-using-frame))
			   #'helm-apropos-short-doc-transformer)))
      :nomark t
      :persistent-action (lambda (candidate)
			   (helm-elisp--persistent-help
			    candidate 'helm-describe-variable))
      :persistent-help "Toggle describe variable"
      :keymap helm-apropos-map
      :action '(("Describe variable" . helm-describe-variable)
		("Find variable" . helm-find-variable)
		("Info lookup" . helm-info-lookup-symbol)
		("Set variable" . helm-set-variable))
      :action-transformer 'helm-apropos-action-transformer))

  (setq helm-apropos-show-short-doc t
	helm-autoresize-max-height 45
	helm-autoresize-min-height 10)
  (defadvice helm-persistent-help-string (around avoid-help-message activate)
    "Avoid help message")
  (defadvice helm-display-mode-line (after undisplay-header activate)
    (setq header-line-format nil))
  (helm-mode 1)
  (helm-autoresize-mode t))

(use-package
  helm-autoloads
  :after helm)

(use-package
  helm-command
  :after helm
  :config
  (dolist (r (list (rx "\*helpful")
		   (rx "\*Async-native-compile-log\*")
		   (rx "\*Warnings\*")
		   (rx "\*Messages\*")
		   (rx "\*Backtrace\*")))
    (add-to-list 'helm-boring-buffer-regexp-list r)))

;; ---------------------------------- Projects ---------------------------------
(use-package
  projectile
  :config
  (setq projectile-project-search-path '("~/priv/code"
					 "~/priv/documents"))
  (projectile-mode 1))

;; --------------------------------- Treesitter --------------------------------
(use-package
  tree-sitter)

(use-package
  tree-sitter-langs
  :after tree-sitter
  :config
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; Symbols
(setq prettify-symbols-unprettify-at-point t)
(add-hook 'prog-mode-hook #'prettify-symbols-mode)

;; ---------------------------------- Snippets ---------------------------------
(use-package
  yasnippet
  :config
  (yas-global-mode 1))

;; ------------------------------------ Org ------------------------------------
(setq org-directory "~/priv/documents/org/")

(use-package
  org-modern
  :hook ((org-mode-hook . org-modern-mode)
	 (org-agenda-finalize-hook . org-modern-mode))
  :config
  (setq rg-auto-align-tags nil
	org-tags-column 0
	org-catch-invisible-edits 'show-and-error
	org-special-ctrl-a/e t
	org-insert-heading-respect-content t

	org-hide-emphasis-markers t
	org-pretty-entities t
	org-agenda-tags-column 0
	org-ellipsis "…"))

;; ----------------------------------- LaTeX -----------------------------------
(with-eval-after-load 'tex
  (setq TeX-PDF-mode t
	TeX-auto-save t
        TeX-source-correlate-mode t
        TeX-source-correlate-start-server nil)
  (setcar (cdr (assoc 'output-pdf TeX-view-program-selection))
	  "Zathura")
  (add-hook 'LaTeX-mode-hook (lambda ()
                               (require 'tex-fold)
                               (TeX-fold-mode 1)

                               (require 'evil-tex)
                               (evil-tex-mode)))
  (add-hook 'find-file-hook #'TeX-fold-buffer t))

;; ---------------------------------- Jupyter ----------------------------------
(use-package
  jupyter
  :config
  (setq jupyter-eval-use-overlays t)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (jupyter . t))))

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
  :config
  (lsp-ui-mode))

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

