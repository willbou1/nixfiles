;s -*- lexical-binding: t; -*-

(require 'use-package)
(require 'lib)
(require 'base16-theme)
(require 'savehist)

(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

(add-to-list 'custom-theme-load-path "~/.config/emacs/themes/")
(load-theme 'base16-stylix t)

(let ((l-modifier (lambda (l) (- l 0.1)))
      (s-modifier (lambda (l) (- l 0.2))))
  (defvar +theme-highlight-window-bg
    (+color-stylix :base01))
  (defvar +theme-highlight-window-bg-inactive
    (+color-stylix :base01 nil s-modifier l-modifier))
  (defvar +theme-highlight-window-line-number-fg
    (+color-stylix :base0D))
  (defvar +theme-highlight-window-line-number-fg-inactive
    (+color-stylix :base0D nil s-modifier l-modifier))
  (defvar +theme-highlight-window-current-line-number-fg
    (+color-stylix :base03))
  (defvar +theme-highlight-window-current-line-number-fg-inactive
    (+color-stylix :base03 nil s-modifier l-modifier)))


;;; Transparency
(+override
 'create-image
 (lambda (original)
   (lambda (&rest args)
     "args = (FILE-OR-DATA &optional TYPE DATA-P &rest PROPS)"
     (let ((props (nthcdr 3 args)))
       (unless (plist-member props :mask)
	 (setq args (append args '(:mask heuristic))))
       (apply original args)))))

(unless +theme-frame-alpha
  (setq +theme-frame-alpha 80))
(setq +theme-completion-alpha (floor (min (* 1.3 +theme-frame-alpha) 100)))

(defun +theme--set-frame-alpha (&optional frame)
  "Set alpha for main frames. Child frames like Corfu get different alpha if desired."
  (let ((frame (or frame (selected-frame))))
    (when (display-graphic-p frame)
      (cond
       ;; Top-level frames
       ((not (frame-parameter frame 'parent-frame))
        (set-frame-parameter frame 'alpha-background +theme-frame-alpha))
       ;; Corfu popups
       ((string-equal (frame-parameter frame 'name) "EmacsCorfuGUI")
        (set-frame-parameter frame 'alpha-background +theme-completion-alpha))
       ;; LSP UI
       ((string-equal (frame-parameter frame 'name) "F1")
        (set-frame-parameter frame 'alpha-background +theme-completion-alpha))
       ;; Other child frames stay fully opaque
       (t
        (set-frame-parameter frame 'alpha-background 100))))))

(+theme--set-frame-alpha)

(add-hook 'after-make-frame-functions
	  (lambda (frame)
	    (+theme--set-frame-alpha frame)))

(defun +theme-set-frame-alpha ()
  "Set the transparency of the Emacs frame."
  (interactive)
  (let ((alpha (read-number "Alpha [0-100]: " +theme-frame-alpha)))
    (when (or (> 0 alpha) (< 100 alpha))
      (error "Alpha must be between 0 and 100"))
    (setq +theme-frame-alpha alpha
	  +theme-completion-alpha (floor (min (* 1.3 +theme-frame-alpha) 100)))
    (dolist (f (frame-list))
      (+theme--set-frame-alpha f))))


;;; Frames
(dolist (face '(window-divider
                 window-divider-first-pixel
                 window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (+color-stylix :base02)))
(set-face-background 'fringe (+color-stylix :base01))
(set-face-foreground 'fringe (+color-stylix :base0F))

(defcustom +theme-gap 2

  "Size of the gaps")
(defcustom +theme-internal-gap 0
           "Size of the internal gaps")
(add-hook 'post-command-hook
          (lambda ()
            (+set-default-frame-parameters
              'right-divider-width +theme-gap
              'bottom-divider-width +theme-gap
              'internal-border-width +theme-internal-gap)
            (dolist (buf (list " *Minibuf-0*" " *Minibuf-1*" " *Echo Area 0*" " *Echo Area 1*"))
              (when (get-buffer buf)
                (with-current-buffer buf
                                     (setq-local face-remapping-alist
                                                 `((default (:background ,(+color-stylix :base01))))))))))


;;; Org
(with-eval-after-load
  'org
  (set-face-background
    'org-block
    (+color-stylix
      :base01
      (lambda (h) (- h 0.01))
      nil
      (lambda (v) (- v 0.15))))

  (set-face-background
    'org-block-begin-line
    (+color-stylix
      :base01
      (lambda (h) (- h 0.01))
      nil
      (lambda (v) (- v 0.1))))

  (set-face-background
    'org-block-end-line
    (+color-stylix
      :base01
      (lambda (h) (- h 0.01))
      nil
      (lambda (v) (- v 0.1)))))


;;; Feebleline
(with-current-buffer (messages-buffer)
                     (setq-local mode-line-format nil))


;;; Fonts and font locking
(custom-set-faces
 '(default ((nil :height 160)))

 `(header-line
   ((t (:box (:color ,(+color-stylix :base0B))))))

 `(aw-leading-char-face
   ((t (:box t :foreground ,(+color-stylix :base09)))))
 `(aw-background-face
   ((t (:foreground ,(+color-stylix :base03 nil nil (lambda (l) (* l 0.75)))))))

 `(font-latex-math-face
   ((t (:foreground ,(+color-stylix :base04)))))

 '(org-level-1 ((t (:height 1.3 :weight bold :inherit outline-1))))
 '(org-level-2 ((t (:height 1.2 :weight bold :inherit outline-2))))
 '(org-level-3 ((t (:height 1.0 :inherit outline-3))))
 '(org-level-4 ((t (:height 0.9 :inherit outline-4))))
 '(org-level-5 ((t (:height 0.8 :inherit outline-5))))

 `(feebleline-file-owner-face
   ((t (:foreground ,(+color-stylix :base0C)))))
 `(feebleline-file-mode-face
   ((t (:foreground ,(+color-stylix :base0D)))))
 `(feebleline-git-face
   ((t (:foreground ,(+color-stylix :base0D)))))

 `(helm-tooltip
   ((t (:background unspecified :foreground ,(+color-stylix :base04)))))
 `(helm-M-x-short-doc
   ((t (:box t :foreground ,(+color-stylix :base04)))))
 `(helm-M-x-key
   ((t (:foreground ,(+color-stylix :base03)))))
 
 '(font-lock-comment-face
   ((t (:family "Liberation Serif" :italic t :height 1.15))))
 '(font-lock-doc-face
   ((t (:family "Liberation Serif" :italic t :height 1.15))))
 '(font-lock-string-face
   ((t (:italic t))))
 `(page-break-lines
   ((t (:inherit default :foreground ,(+color-stylix :base03 nil nil (lambda (l) (* l 0.45)))))))

 '(line-number              ((t (:inherit default))))
 '(line-number-current-line ((t (:inherit default :weight bold))))

 `(Man-overstrike
   ((t (:inherit 'bold :foreground ,(+color-stylix :base03)))))
 `(woman-bold
   ((t (:inherit 'bold :foreground ,(+color-stylix :base03)))))

 `(minimap-active-region-background
   ((t (:background ,(+color-stylix :base01)))))
 `(minimap-current-line-face
   ((t (:background ,(+color-stylix :base0A)))))

 `(ement-room-list-favourite
   ((t (:family ,(face-attribute 'default :family) :foreground ,(+color-stylix :base04)))))
 `(ement-tabulated-room-list-favourite
   ((t (:underline t :family ,(face-attribute 'default :family) :foreground ,(+color-stylix :base04)))))
 )

(font-lock-add-keywords
 'emacs-lisp-mode
 '(("#'" . 'font-lock-function-call-face)))


;;; Ligatures
(use-package
  ligature
  :config
  (ligature-set-ligatures 'prog-mode
			  '(;; == === ==== => =| =>>=>=|=>==>> ==< =/=//=// =~
			    ;; =:= =!=
			    ("=" (rx (+ (or ">" "<" "|" "/" "~" ":" "!" "="))))
			    ;; ;; ;;;
			    (";" (rx (+ ";")))
			    ;; && &&&
			    ("&" (rx (+ "&")))
			    ;; !! !!! !. !: !!. != !== !~
			    ("!" (rx (+ (or "=" "!" "\." ":" "~"))))
			    ;; ?? ??? ?:  ?=  ?.
			    ("?" (rx (or ":" "=" "\." (+ "?"))))
			    ;; %% %%%
			    ("%" (rx (+ "%")))
			    ;; |> ||> |||> ||||> |] |} || ||| |-> ||-||
			    ;; |->>-||-<<-| |- |== ||=||
			    ;; |==>>==<<==<=>==//==/=!==:===>
                            ("|" (rx (+ (or ">" "<" "|" "/" ":" "!" "}" "\]"
                                            "-" "=" ))))
                            ;; \\ \\\ \/
                            ("\\" (rx (or "/" (+ "\\"))))
                            ;; ++ +++ ++++ +>
                            ("+" (rx (or ">" (+ "+"))))
                            ;; :: ::: :::: :> :< := :// ::=
                            (":" (rx (or ">" "<" "=" "//" ":=" (+ ":"))))
                            ;; // /// //// /\ /* /> /===:===!=//===>>==>==/
                            ("/" (rx (+ (or ">"  "<" "|" "/" "\\" "\*" ":" "!"
                                            "="))))
                            ;; .. ... .... .= .- .? ..= ..<
                            ("\." (rx (or "=" "-" "\?" "\.=" "\.<" (+ "\."))))
                            ;; -- --- ---- -~ -> ->> -| -|->-->>->--<<-|
                            ("-" (rx (+ (or ">" "<" "|" "~" "-"))))
                            ;; *> */ *)  ** *** ****
                            ("*" (rx (or ">" "/" ")" (+ "*"))))
                            ;; www wwww
                            ("w" (rx (+ "w")))
                            ;; <> <!-- <|> <: <~ <~> <~~ <+ <* <$ </  <+> <*>
                            ;; <$> </> <|  <||  <||| <|||| <- <-| <-<<-|-> <->>
                            ;; <<-> <= <=> <<==<<==>=|=>==/==//=!==:=>
                            ;; << <<< <<<<
                            ("<" (rx (+ (or "\+" "\*" "\$" "<" ">" ":" "~"  "!"
                                            "-"  "/" "|" "="))))
                            ;; >: >- >>- >--|-> >>-|-> >= >== >>== >=|=:=>>
                            ;; >> >>> >>>>
                            (">" (rx (+ (or ">" "<" "|" "/" ":" "=" "-"))))
                            ;; #: #= #! #( #? #[ #{ #_ #_( ## ### #####
                            ("#" (rx (or ":" "=" "!" "(" "\?" "\[" "{" "_(" "_"
                                         (+ "#"))))
                            ;; ~~ ~~~ ~=  ~-  ~@ ~> ~~>
                            ("~" (rx (or ">" "=" "-" "@" "~>" (+ "~"))))
                            ;; __ ___ ____ _|_ __|____|_
                            ("_" (rx (+ (or "_" "|"))))
                            ;; Fira code: 0xFF 0x12
                            ("0" (rx (and "x" (+ (in "A-F" "a-f" "0-9")))))
                            ;; Fira code:
                            "Fl"  "Tl"  "fi"  "fj"  "fl"  "ft"
                            ;; The few not covered by the regexps.
                            "{|"  "[|"  "]#"  "(*"  "}#"  "$>"  "^="))
  (global-ligature-mode t))


;;; Syntax highlighting
(use-package
  rainbow-delimiters
  :hook (prog-mode-hook . rainbow-delimiters-mode)
  :config
  (defun +rainbow-delimiters-pick-face (depth match _loc)
    "Return a vibrant face for DEPTH, cycling through hues every 20°."
    (let* (
	   (hue-step (/ 740 rainbow-delimiters-max-face-count))
	   (hue (mod (* depth hue-step) 360))
	   (s 0.5)
	   (l 0.55)
	   (color (apply #'color-rgb-to-hex
			 (append (color-hsl-to-rgb
				  (+color-remove-bg-from-hue
				   (mod (+ 0.4 (/ hue 360.0)) 1.0) 0.06)
				  s l)
				 '(2)))))
      (let ((face (intern
		   (if match
		       (format "+rainbow-delimiters-depth-%d-face" depth)
		     "+rainbow-delimiters-unmatched-face"))))
	(unless (facep face)
	  (make-face face))
	(set-face-foreground face (if match color "red"))
	face)))
  (setq rainbow-delimiters-pick-face-function #'+rainbow-delimiters-pick-face
	rainbow-delimiters-max-face-count 15))

(use-package
  rainbow-identifiers
  :hook (prog-mode-hook . rainbow-identifiers-mode)
  :init
  (setq rainbow-identifiers-choose-face-function 'rainbow-identifiers-cie-l*a*b*-choose-face
	rainbow-identifiers-cie-l*a*b*-lightness 62
	rainbow-identifiers-cie-l*a*b*-saturation 20))

(use-package
  highlight-defined
  :hook (prog-mode-hook . highlight-defined-mode))

(use-package
  highlight-numbers
  :hook (prog-mode-hook . highlight-numbers-mode))


;;; Dired
(use-package
  diredfl
  :hook (dired-mode . diredfl-mode))

(with-eval-after-load 'dired-subtree
  (setq dired-subtree-line-prefix "")
  (dotimes (i 6)
    (let ((bg (+color-stylix :base01 nil nil
				    (lambda (l) (- l 0.15 (* i 0.03))))))
      (set-face-attribute (intern (format "dired-subtree-depth-%d-face" (+ 1 i))) t
			  :background bg
			  :extend t))))


;;; Ultrawide
(use-package
  olivetti
  :config
  (setq-default olivetti-body-width 130))


;;; Smooth scrolling
(use-package
  smooth-scroll
  :config
  (setq smooth-scroll/vscroll-step-size 2)
  (add-hook 'server-after-make-frame-hook #'smooth-scroll-mode))


;;; EShell
(with-eval-after-load 'eshell
  (require 'ansi-color)
  (defun eshell-handle-ansi-color ()
    (ansi-color-apply-on-region eshell-last-output-start
				eshell-last-output-end))
  (add-to-list 'eshell-output-filter-functions 'eshell-handle-ansi-color)

  (setq eshell-prompt-function
	(lambda ()
          (concat
           (propertize (user-login-name)
                       'face `(:foreground ,(+color-stylix :base0A)))
           (if (eq (system-name) "linux-amd")
               "@"
	     "")
           (if (eq (system-name) "linux-amd")
	       (propertize (system-name)
			   'face '(:foreground "cyan"))
	     "")
           ":"
           (propertize (abbreviate-file-name (eshell/pwd))
                       'face `(:foreground ,(+color-stylix :base0E)))
           (propertize (if (= (user-uid) 0) " # " " $ ")
		       'face `(:foreground ,(+color-stylix :base09)))))))


;;; Dynamic window shading
(defvar +theme-last-selected-window nil
  "The previously active window for theme highlighting.")
(defvar +theme-last-selected-buffer nil
  "The previously active buffer for theme highlighting.")
(defvar +theme-active-window-remaps nil)

(defun +theme-highlight-window ()
  "Highlight the currently selected window differently from the last selected one."
  (let* ((current-window (selected-window))
	 (current-buffer (window-buffer current-window))
         (last-window +theme-last-selected-window)
         (last-buffer +theme-last-selected-buffer)
         ;; Colors for active/inactive
         (bg-active +theme-highlight-window-bg)
         (bg-inactive +theme-highlight-window-bg-inactive)
         (ln-fg-active +theme-highlight-window-line-number-fg)
         (ln-fg-inactive +theme-highlight-window-line-number-fg-inactive)
         (ln-cur-fg-active +theme-highlight-window-current-line-number-fg)
         (ln-cur-fg-inactive +theme-highlight-window-current-line-number-fg-inactive))
    
    ;; Restore the last window to inactive
    (when (and (window-live-p last-window) (buffer-live-p last-buffer))
      (with-current-buffer last-buffer
        (when +theme-active-window-remaps
          (mapc #'face-remap-remove-relative +theme-active-window-remaps))
        (setq +theme-active-window-remaps
              (list
               (face-remap-add-relative 'window-divider :foreground bg-inactive)
               (face-remap-add-relative 'window-divider-first-pixel :foreground bg-inactive)
               (face-remap-add-relative 'window-divider-last-pixel :background bg-inactive)
               (face-remap-add-relative 'fringe :background bg-inactive)

	       (face-remap-add-relative 'header-line :background bg-inactive)

               (face-remap-add-relative 'line-number
                                        :background bg-inactive
                                        :foreground ln-fg-inactive
                                        :extend t)
               (face-remap-add-relative 'line-number-current-line
                                        :background bg-inactive
                                        :foreground ln-cur-fg-inactive
                                        :extend t)
               (face-remap-add-relative 'hl-line
                                        :background bg-inactive
                                        :extend t)))))

    ;; Highlight the current window as active
    (when (and (window-live-p current-window) (buffer-live-p current-buffer))
      (with-current-buffer current-buffer
        (when +theme-active-window-remaps
          (mapc #'face-remap-remove-relative +theme-active-window-remaps))
        (setq +theme-active-window-remaps
              (list
               (face-remap-add-relative 'window-divider :foreground bg-active)
               (face-remap-add-relative 'window-divider-first-pixel :foreground bg-active)
               (face-remap-add-relative 'window-divider-last-pixel :background bg-active)
               (face-remap-add-relative 'fringe :background bg-active)

	       (face-remap-add-relative 'header-line :background bg-active)
	       
               (face-remap-add-relative 'line-number
                                        :background bg-active
                                        :foreground ln-fg-active
                                        :extend t)
               (face-remap-add-relative 'line-number-current-line
                                        :background bg-active
                                        :foreground ln-cur-fg-active
                                        :extend t)
               (face-remap-add-relative 'hl-line
                                        :background bg-active
                                        :extend t)))))

    ;; Update last selected window
    (setq +theme-last-selected-window current-window
	  +theme-last-selected-buffer current-buffer)))

(add-hook 'window-selection-change-functions
          (lambda (&rest _) (+theme-highlight-window)))
(add-hook 'buffer-list-update-hook #'+theme-highlight-window)

(provide 'theme)
