;; -*- lexical-binding: t; -*-

(require 'use-package)
(require 'lib)
(require 'base16-theme)
(require 'savehist)

;; -------------------------------- Base16 theme -------------------------------
(add-to-list 'custom-theme-load-path "~/.config/emacs/themes/")
(load-theme 'base16-stylix t)

; ------------------------ Disabling the stock Emacs UI ------------------------
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; -------------------------------- Transparency -------------------------------
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

(+set-default-frame-parameters 'alpha-background +theme-frame-alpha)

(defun +theme-set-frame-alpha ()
  "Set the transparency of the Emacs frame."
  (interactive)
  (let ((alpha (read-number "Alpha [0-100]: " +theme-frame-alpha)))
	      (when (or (> 0 alpha) (< 100 alpha))
		(error "Alpha must be between 0 and 100"))
	      (setq +theme-frame-alpha alpha)
          (+set-default-frame-parameters 'alpha-background +theme-frame-alpha)))

;; ----------------------------------- Frames ----------------------------------

(dolist (face '(window-divider
                 window-divider-first-pixel
                 window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (plist-get base16-stylix-theme-colors :base02)))
(set-face-background 'fringe (plist-get base16-stylix-theme-colors :base01))

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
                                                 `((default (:background ,(plist-get base16-stylix-theme-colors
                                                                                     :base01))))))))))

;; ------------------------------------ Org ------------------------------------
(with-eval-after-load
  'org
  (set-face-background
    'org-block
    (+color-modify-stylix
      :base01
      (lambda (h) (- h 0.01))
      nil
      (lambda (v) (- v 0.15))))

  (set-face-background
    'org-block-begin-line
    (+color-modify-stylix
      :base01
      (lambda (h) (- h 0.01))
      nil
      (lambda (v) (- v 0.1))))

  (set-face-background
    'org-block-end-line
    (+color-modify-stylix
      :base01
      (lambda (h) (- h 0.01))
      nil
      (lambda (v) (- v 0.1))))

  (set-face-attribute 'org-level-1 nil :height 1.3 :weight 'bold)
  (set-face-attribute 'org-level-2 nil :height 1.2 :weight 'bold)
  (set-face-attribute 'org-level-3 nil :height 1.0)
  (set-face-attribute 'org-level-4 nil :height 0.9)
  (set-face-attribute 'org-level-5 nil :height 0.8))

;; --------------------------------- Feebleline --------------------------------
(with-eval-after-load
  'feebleline
  (set-face-foreground 'feebleline-file-owner-face (plist-get base16-stylix-theme-colors :base0C))
  (set-face-foreground 'feebleline-file-mode-face (plist-get base16-stylix-theme-colors :base0D))
  (set-face-foreground 'feebleline-git-face (plist-get base16-stylix-theme-colors :base0D)))

(with-current-buffer (messages-buffer)
                     (setq-local mode-line-format nil))

;; ------------------------------------ Helm -----------------------------------
(with-eval-after-load
  'helm-command
  (set-face-foreground 'helm-M-x-short-doc (plist-get base16-stylix-theme-colors :base0F))
  (set-face-foreground 'helm-M-x-key (plist-get base16-stylix-theme-colors :base03)))

;; --------------------------- Fonts and font locking --------------------------
(set-face-attribute 'default nil :height 185)

(custom-set-faces
 '(font-lock-comment-face
   ((t (:family "Liberation Serif" :italic t :height 1.15))))
 '(font-lock-doc-face
   ((t (:family "Liberation Serif" :italic t :height 1.15))))
 '(font-lock-string-face  ((t (:italic t))))
 '(line-number
   ((t (:inherit default))))
 '(line-number-current-line
   ((t (:inherit default :weight bold)))))

(font-lock-add-keywords
 'emacs-lisp-mode
 '(("#'" . 'font-lock-function-call-face)))

(with-eval-after-load 'man
  (set-face-attribute 'Man-overstrike nil :inherit 'bold :foreground (plist-get base16-stylix-theme-colors :base03)))
(with-eval-after-load 'woman
  (set-face-attribute 'woman-bold nil :inherit 'bold :foreground (plist-get base16-stylix-theme-colors :base03)))

;; --------------------------------- Ligatures ---------------------------------
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

;; ---------------------------- Syntax highlighting ----------------------------
(use-package
  rainbow-delimiters
  :hook (prog-mode-hook . rainbow-delimiters-mode)
  :config
  (defun +rainbow-delimiters-pick-face (depth match _loc)
    "Return a vibrant face for DEPTH, cycling through hues every 20°."
    (let* ((hue-step (/ 720 rainbow-delimiters-max-face-count))
	   (hue (mod (* depth hue-step) 360))
	   (s 0.6)
	   (l 0.4)
	   (color (apply #'color-rgb-to-hex
			 (append (color-hsl-to-rgb (/ hue 360.0) s l) '(2)))))
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
  :hook (prog-mode-hook . rainbow-identifiers-mode))

(use-package
  highlight-defined
  :hook (prog-mode-hook . highlight-defined-mode))

(use-package
  highlight-numbers
  :hook (prog-mode-hook . highlight-numbers-mode))

(with-eval-after-load
  'font-latex
  (set-face-foreground 'font-latex-math-face (plist-get base16-stylix-theme-colors :base04)))

;; ----------------------------------- Dired -----------------------------------
(use-package
  diredfl
  :hook (dired-mode . diredfl-mode))

(with-eval-after-load 'dired-subtree
  (setq dired-subtree-line-prefix "")
  (dotimes (i 6)
    (let ((bg (+color-modify-stylix :base01
				    nil
				    (lambda (s) (- s 0.1))
				    (lambda (l) (- l (* i -0.06))))))
      (set-face-attribute (intern (format "dired-subtree-depth-%d-face" (+ 1 i))) nil
			  :background bg
			  :extend t))))

;; --------------------------------- Ultrawide ---------------------------------
(use-package
  olivetti
  :config
  (setq-default olivetti-body-width 130))

;; ------------------------------ Smooth scrolling -----------------------------
(use-package
  smooth-scroll
  :config
  (setq smooth-scroll/vscroll-step-size 2)
  (add-hook 'server-after-make-frame-hook #'smooth-scroll-mode))

;; ----------------------------------- EShell ----------------------------------
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
                       'face `(:foreground ,(plist-get base16-stylix-theme-colors :base0A)))
           (if (eq (system-name) "linux-amd")
               "@"
	     "")
           (if (eq (system-name) "linux-amd")
	       (propertize (system-name)
			   'face '(:foreground "cyan"))
	     "")
           ":"
           (propertize (abbreviate-file-name (eshell/pwd))
                       'face `(:foreground ,(plist-get base16-stylix-theme-colors :base0E)))
           (propertize (if (= (user-uid) 0) " # " " $ ")
		       'face `(:foreground ,(plist-get base16-stylix-theme-colors :base09)))))))

;; --------------------------------- Ace Window --------------------------------
(with-eval-after-load 'ace-window
  (set-face-attribute
   'aw-leading-char-face nil
   :box t
   :foreground (plist-get base16-stylix-theme-colors :base09)))

;; --------------------------- Dynamic window shading --------------------------
(let ((l-modifier (lambda (l) (- l 0.1)))
      (s-modifier (lambda (l) (- l 0.2))))
  (defvar +theme-highlight-window-bg
    (+color-modify-stylix :base01 nil nil nil))
  (defvar +theme-highlight-window-bg-inactive
    (+color-modify-stylix :base01 nil s-modifier l-modifier))
  (defvar +theme-highlight-window-line-number-fg
    (+color-modify-stylix :base0D nil nil nil))
  (defvar +theme-highlight-window-line-number-fg-inactive
    (+color-modify-stylix :base0D nil s-modifier l-modifier))
  (defvar +theme-highlight-window-current-line-number-fg
    (+color-modify-stylix :base03 nil nil nil))
  (defvar +theme-highlight-window-current-line-number-fg-inactive
    (+color-modify-stylix :base03 nil s-modifier l-modifier)))

(defvar +theme-last-selected-window nil
  "The previously active window for theme highlighting.")
(defvar +theme-active-window-remaps nil)

(defun +theme-highlight-window ()
  "Highlight the currently selected window differently from the last selected one."
  (let* ((current (selected-window))
         (last +theme-last-selected-window)
         ;; Colors for active/inactive
         (bg-active +theme-highlight-window-bg)
         (bg-inactive +theme-highlight-window-bg-inactive)
         (ln-fg-active +theme-highlight-window-line-number-fg)
         (ln-fg-inactive +theme-highlight-window-line-number-fg-inactive)
         (ln-cur-fg-active +theme-highlight-window-current-line-number-fg)
         (ln-cur-fg-inactive +theme-highlight-window-current-line-number-fg-inactive))
    
    ;; Restore the last window to inactive
    (when (window-live-p last)
      (with-current-buffer (window-buffer last)
        (when +theme-active-window-remaps
          (mapc #'face-remap-remove-relative +theme-active-window-remaps))
        (setq +theme-active-window-remaps
              (list
               (face-remap-add-relative 'window-divider :foreground bg-inactive)
               (face-remap-add-relative 'window-divider-first-pixel :foreground bg-inactive)
               (face-remap-add-relative 'window-divider-last-pixel :background bg-inactive)
               (face-remap-add-relative 'fringe :background bg-inactive)
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
    (when (window-live-p current)
      (with-current-buffer (window-buffer current)
        (when +theme-active-window-remaps
          (mapc #'face-remap-remove-relative +theme-active-window-remaps))
        (setq +theme-active-window-remaps
              (list
               (face-remap-add-relative 'window-divider :foreground bg-active)
               (face-remap-add-relative 'window-divider-first-pixel :foreground bg-active)
               (face-remap-add-relative 'window-divider-last-pixel :background bg-active)
               (face-remap-add-relative 'fringe :background bg-active)
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
    (setq +theme-last-selected-window current)))

(add-hook 'window-selection-change-functions
          (lambda (&rest _) (+theme-highlight-window)))
(add-hook 'buffer-list-update-hook #'+theme-highlight-window)

(provide 'theme)
