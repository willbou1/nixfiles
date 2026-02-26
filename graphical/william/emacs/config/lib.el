;; -*- lexical-binding: t; -*-

(require 's)
(require 'dash)
(require 'color)

(defun +obj-to-string (obj)
  "Prints an object to a string handling escape sequences"
  (s-replace-all (append '(("\n" . "\\n")
			   ("\t" . "\\t")
			   ("\r" . "\\r"))
			 (mapcar (lambda (c) (cons (string (+ ?\C-a c))
					      (string ?^ (+ ?A c))))
				 (number-sequence 0 25)))
		 (prin1-to-string obj)))

(defun +override (original-symbol replacement)
  "Replace a builtin elisp function with a wrapper."
  (let ((original-cell (symbol-function original-symbol)))
    (setf (symbol-function original-symbol)
	  (funcall replacement original-cell))))

(defun +describe-foo-at-point ()
  "Show the documentation of the Elisp function and variable near point.
This checks in turn:

-- for a function name where point is
-- for a variable name where point is
-- for a surrounding function call
"
  (interactive)
  (let (sym)
    ;; sigh, function-at-point is too clever.  we want only the first half.
    (cond ((setq sym (ignore-errors
                       (with-syntax-table emacs-lisp-mode-syntax-table
                         (save-excursion
                           (or (not (zerop (skip-syntax-backward "_w")))
                               (eq (char-syntax (char-after (point))) ?w)
                               (eq (char-syntax (char-after (point))) ?_)
                               (forward-sexp -1))
                           (skip-chars-forward "`'")
                           (let ((obj (read (current-buffer))))
                             (and (symbolp obj) (fboundp obj) obj))))))
           (helpful-function sym))
          ((setq sym (variable-at-point)) (helpful-variable sym))
          ;; now let it operate fully -- i.e. also check the
          ;; surrounding sexp for a function call.
          ((setq sym (function-at-point)) (helpful-function sym)))))

(defun +insert-section (text)
  "Insert a comment wrapping the prompted text in hyphens."
  (interactive "M")
  (let* ((trimmed-text (s-trim text))
	 (nb-hyphens (- 80 (length trimmed-text) 5))
	 (nb-remainder (mod nb-hyphens 2))
	 (nb-quotient (/ nb-hyphens 2)))
    (insert
     (concat ";; "
	     (s-repeat (+ nb-quotient nb-remainder) "-")
	     " " trimmed-text " "
	     (s-repeat nb-quotient "-")))))

(defun +set-default-frame-parameters (&rest params)
  "Set the specified default frame parameters and apply them instantly."
  (when (cl-oddp (length params))
    (error "The number of parameters must be even!"))
  (while params
    (let ((k (pop params))
          (v (pop params)))
      (setf (alist-get k default-frame-alist) v)
      (set-frame-parameter nil k v))))

;; ----------------------------------- Colors ----------------------------------
(defun +color-hex-to-rgb (hex-color)
  "Converts a value like #RRGGBB to rgb"
  (mapcar (lambda (component) (/ (float (string-to-number component 16)) 255))
	  (list (substring hex-color 1 3)
		(substring hex-color 3 5)
		(substring hex-color 5 7))))

(defun +color-modify-hex (color hf sf lf)
  "Modify a hex color with HSL modifiers"
  (defun clamp (x)
    (max 0.0 (min 1.0 x)))
  (let* ((color-rgb (+color-hex-to-rgb color))
	 (color-hsl (apply #'color-rgb-to-hsl color-rgb)))
    (unless (null hf)
      (setf (nth 0 color-hsl)
	    (clamp (funcall hf (nth 0 color-hsl)))))
    (unless (null sf)
      (setf (nth 1 color-hsl)
	    (clamp (funcall sf (nth 1 color-hsl)))))
    (unless (null lf)
      (setf (nth 2 color-hsl)
	    (clamp (funcall lf (nth 2 color-hsl)))))
    (apply #'color-rgb-to-hex (append (apply #'color-hsl-to-rgb color-hsl) '(2)))))

(defun +color-modify-stylix (color hf sf lf)
  "Modify a stylix color with HSL modifiers"
  (+color-modify-hex (plist-get base16-stylix-theme-colors color) hf sf lf))

(defun +color-remove-bg-from-hue (hue theta)
  "Transform HUE [0, 1] to exclude the hue of the background symmetrically. The size of the exclusion regino
is two times THETA."
  (let ((bg-hue (car (apply #'color-rgb-to-hsl
			    (+color-hex-to-rgb
			     (plist-get base16-stylix-theme-colors :base00))))))
    (mod (* (- 1 (* 2 theta))
	    (+ hue bg-hue theta))
	 1.0)))

;; ----------------------------- Buffers & Windows -----------------------------
(defun +visible-buffer-windows ()
  (let ((selected-window (selected-window)))
    (mapcar (lambda (g)
	      (let* ((buffer (car g))
		     (windows (mapcar #'cdr (cdr g)))
		     (activep (not (null (any (lambda (w) (eq w selected-window)) windows)))))
		(append
		 (list buffer activep)
		 windows)))
	    (seq-group-by #'car
			  (mapcar (lambda (w)
				    (cons (window-buffer w)
					  w))
				  (window-list))))))


(provide 'lib)
