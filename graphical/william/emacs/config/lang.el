;; -*- lexical-binding: t; -*-

;; LaTeX
(use-package tex-site
  :ensure auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :init
  (setq TeX-PDF-mode t
        TeX-auto-save t
        TeX-parse-self t
        TeX-source-correlate-start-server t
	TeX-source-correlate-method 'synctex
	TeX-view-program-selection
	'((output-pdf "Zathura")))
  :config
  (add-hook 'LaTeX-mode-hook #'TeX-source-correlate-mode))

(use-package
  evil-tex
  :hook (LaTeX-mode . evil-tex-mode))

(use-package
  tex-fold
  :init
  (setq TeX-fold-auto t)
  :hook
  (LaTeX-mode . tex-fold-mode)
  (LaTeX-mode . (lambda ()
                  (run-with-idle-timer 0.1 nil #'TeX-fold-buffer))))


;; LSP
(add-to-list 'auto-mode-alist '("\\.yuck\\'" . lisp-data-mode))

(use-package
  lsp-mode
  :hook ((c-ts-mode . lsp-deferred)
	 (c++-ts-mode . lsp-deferred)
	 (typescript-ts-mode . lsp-deferred)
	 (nix-mode . lsp-deferred)
	 (haskell-mode . lsp-deferred)
	 (rust-ts-mode . lsp-deferred)
	 (LaTeX-mode . lsp-deferred)))

(use-package
  lsp-pyright
  :after lsp-mode
  :custom (lsp-pyright-langserver-command "pyright")
  :hook (python-ts-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

(defvar +lsp-ui-imenu--remaps nil)
(use-package
  lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :init
  (setq lsp-ui-imenu-window-fix-width t
	lsp-ui-imenu-auto-refresh t
	lsp-ui-imenu-window-width 25

	lsp-ui-doc-show-with-cursor nil
	lsp-ui-doc-alignment 'window
	lsp-ui-doc-border (plist-get base16-stylix-theme-colors :base09)
	lsp-ui-doc-delay 1.0
	lsp-ui-doc-use-webkit t
	lsp-ui-doc-include-signature t)
  :config
  (add-hook 'lsp-ui-imenu-mode-hook
	    (lambda ()
	      (with-current-buffer "*lsp-ui-imenu*"
		(when +lsp-ui-imenu--remaps
		  (face-remap-remove-relative +lsp-ui-imenu--remaps))
		(setq +lsp-ui-imenu--remaps
		      (face-remap-add-relative 'default :height 0.55))))))


;; DAP
(use-package dap-mode
  :init
  (setq dap-auto-configure-features '(sessions locals controls tooltip)))

(use-package
  dap-lldb
  :after dap-mode
  :hook (c++-ts-mode . (lambda () (require 'dap-lldb)))
  :config
  (setq dap-lldb-debug-program '("lldb-dap")))

(use-package
  dap-python
  :after dap-mode
  :hook (python-ts-mode . (lambda () (require 'dap-python))))

(require 'feebleline)
(setq feebleline-timer-interval 0.1)
(feebleline-mode 1)


;; Tools to deal with horrible people who designed a language in which the blocks are based
;; on indentation for fuck's sake lmao
(use-package indent-bars
  :hook ((python-ts-mode yaml-ts-mode) . indent-bars-mode)
  :init
  (setq indent-bars-no-descend-lists 'skip
	indent-bars-treesit-support t
	indent-bars-treesit-ignore-blank-lines-types '("module")
	indent-bars-treesit-scope '((python function_definition class_definition for_statement
					    if_statement with_statement while_statement))))

(provide 'lang)
