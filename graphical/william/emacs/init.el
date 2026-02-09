
;; ------------------------------------- GC ------------------------------------
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)
(run-with-idle-timer
 5 t
 (lambda ()
   (setq gc-cons-threshold (* 64 1024 1024)))) ;; 64MB
(add-hook 'pre-command-hook (lambda ()
			      (setq gc-cons-threshold most-positive-fixnum)))

;; --------------------------------- Load paths --------------------------------
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

(make-directory "~/.config/emacs/playground/" t)
(add-to-list 'load-path (expand-file-name "playground" user-emacs-directory))

(savehist-mode 1)
(setq-default warning-minimum-level :error)

(require 'core)

(let ((playground-file "~/.config/emacs/playground/playground.el"))
  (if (file-exists-p playground-file)
    (load playground-file)))

(setq inhibit-startup-screen t)
