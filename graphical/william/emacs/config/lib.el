;; -*- lexical-binding: t; -*-

(require 's)
(require 'dash)

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

(provide 'lib)
