;; -*- lexical-binding: t; -*-

;; ------------------------------------ Helm -----------------------------------
(use-package
  helm
  :config

  (setq helm-apropos-show-short-doc t
	helm-M-x-show-short-doc t
	helm-autoresize-max-height 45
	helm-autoresize-min-height 30
	helm-locate-command "locate -e -r %s"
	helm-candidate-number-limit 125
	helm-input-idle-delay 0.1
	helm-display-function
	(lambda (buf &rest args)
	  "Always show Helm in the bottom half of the frame."
	  (let* ((frame (selected-frame))
		 (root-win (frame-root-window frame))
		 (bottom-height (/ (frame-height frame) 2))
		 (bottom-win (split-window root-win (- bottom-height) 'below)))
	    (set-window-buffer bottom-win buf)
	    bottom-win)))
  (defadvice helm-persistent-help-string (around avoid-help-message activate)
    "Avoid help message")
  (defadvice helm-display-mode-line (after undisplay-header activate)
    (setq header-line-format nil))

  (fset 'helm-display-mode-line #'ignore)
  (add-hook 'helm-after-initialize-hook
            (defun hide-mode-line-in-helm-buffer ()
              "Hide mode line in `helm-buffer'."
              (with-helm-buffer
                (setq-local mode-line-format nil))))

  (helm-mode 1)
  (helm-autoresize-mode t)
  (helm-popup-tip-mode 1))

(use-package
  helm-autoloads
  :after helm)

(use-package
  helm-command
  :after helm
  :config
  (defun helm-apropos-short-doc-transformer (candidates source)
    (if helm-apropos-show-short-doc
	(let* ((width (window-width (get-buffer-window (helm-buffer-get))))
	       (cand-max-length (ceiling (* 0.35 width)))
	       (val-max-length (ceiling (* 0.17 width)))
	       (padding 3)
	       (cand-max-length-with-padding (+ padding cand-max-length))
	       (val-max-length-with-padding (+ padding val-max-length)))
	  (cl-loop for cand in candidates
		   for canonical = (intern-soft cand)
		   for doc = (helm-get-first-line-documentation canonical)
		   for variablep = (equal "Variables" (alist-get 'name source))
		   for shrunk-cand = (truncate-string-to-width cand cand-max-length 0 nil t)
		   for val = (if variablep
				 (truncate-string-to-width (+obj-to-string (symbol-value canonical))
							   val-max-length 0 nil t))
		   collect (cons (format "%s%s%s%s%s"
					 shrunk-cand
					 (if (or val doc)
					     (helm-make-separator shrunk-cand cand-max-length-with-padding)
					   "")
					 (if val
					     (propertize
					      val 'face 'helm-M-x-key)
					   "")
					 (if val
					     (helm-make-separator val val-max-length-with-padding)
					   "")
					 (if doc
					     (propertize
					      doc 'face 'helm-M-x-short-doc)
					   ""))
				 cand)))
      candidates))


  (dolist (r (list (rx "\*Async-native-compile-log\*")
		   (rx "\*Warnings\*")
		   (rx "\*WoMan-Log\*")
		   (rx "\*Messages\*")
		   (rx "\*Help\*")
		   (rx "\*dashboard\*")
		   (rx "\*scratch\*")
		   (rx "\*direnv\*")
		   (rx "\*lsp-log\*")
		   (rx "\*lsp-ui-imenu\*")
		   (rx "\*nixd-lsp\*")
		   (rx "\*Backtrace\*")
           (rx "\*Org Preview LaTeX Output\*")))
    (add-to-list 'helm-boring-buffer-regexp-list r)))

(provide 'menu)
