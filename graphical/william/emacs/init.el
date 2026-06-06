;; ------------------------------------- GC ------------------------------------
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 64 1024 1024)
                  gc-cons-percentage 0.1)))

(add-hook 'minibuffer-setup-hook (lambda () (setq gc-cons-threshold most-positive-fixnum)))
(add-hook 'minibuffer-exit-hook  (lambda () (setq gc-cons-threshold (* 64 1024 1024))))

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

