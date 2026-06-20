;; ------------------------------------- GC ------------------------------------
(use-package
  gcmh
  :config
  (gcmh-mode 1))

;; --------------------------------- Load paths --------------------------------
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

(make-directory "~/.config/emacs/playground/" t)
(add-to-list 'load-path (expand-file-name "playground" user-emacs-directory))

(setq-default warning-minimum-level :error)
(setq inhibit-startup-screen t)

(require 'core)
(require 'lib)

(let ((playground-file (expand-file-name "playground/playground.el" user-emacs-directory)))
  (when (file-exists-p playground-file)
    (load playground-file t)))

