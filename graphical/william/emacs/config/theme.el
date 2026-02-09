;; -*- lexical-binding: t; -*-

(require 'use-package)
(require 'lib)
(require 'base16-theme)

;; -------------------------------- Base16 theme -------------------------------
(add-to-list 'custom-theme-load-path "~/.config/emacs/themes/")
(load-theme 'base16-stylix t)

; ------------------------ Disabling the stock Emacs UI ------------------------
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; -------------------------------- Transparency -------------------------------
;; This is a hugeeeee hack to force images to render with a masking heuristic,
;; enforcing alpha transparency
(+override
 'create-image
 (lambda (original)
   (lambda (&rest args)
     "args = (FILE-OR-DATA &optional TYPE DATA-P &rest PROPS)"
     (let ((props (nthcdr 3 args)))
       (unless (plist-member props :mask)
	 (setq args (append args '(:mask heuristic))))
       (apply original args)))))

(setq +theme-frame-alpha 74)
(add-to-list 'savehist-additional-variables 'frame-alpha)
(set-frame-parameter nil 'alpha-background 74)
(add-to-list 'default-frame-alist '(alpha-background . 74))

(defun +theme-set-frame-alpha ()
  "Set the transparency of the Emacs frame."
  (interactive)
  (let ((alpha (read-number "Alpha [0-100]: " +theme-frame-alpha)))
	      (when (or (> 0 alpha) (< 100 alpha))
		(error "Alpha must be between 0 and 100"))
	      (setq +theme-frame-alpha alpha)
	      (set-frame-parameter nil 'alpha-background alpha)
	      (add-to-list 'default-frame-alist `(alpha-background . ,alpha))))

;; ----------------------------------- Frames ----------------------------------
(modify-all-frames-parameters
 '((right-divider-width . 1)
   (bottom-divider-width . 1)
   (internal-border-width . 0)))

(dolist (face '(window-divider
		window-divider-first-pixel
		window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (plist-get base16-stylix-theme-colors :base02)))
(set-face-background 'fringe (plist-get base16-stylix-theme-colors :base01))
(add-hook 'post-command-hook
	  (lambda () (dolist (buf (list " *Minibuf-0*" " *Minibuf-1*" " *Echo Area 0*" " *Echo Area 1*"))
		  (when (get-buffer buf)
		    (with-current-buffer buf
		      (setq-local face-remapping-alist
				  `((default (:background ,(plist-get base16-stylix-theme-colors
								      :base01))))))))))

;; --------------------------------- Feebleline --------------------------------
(with-eval-after-load
  'feebleline
  (set-face-foreground 'feebleline-git-face (plist-get base16-stylix-theme-colors :base0D)))

;; ------------------------------------ Helm -----------------------------------
(with-eval-after-load
  'helm-command
  (set-face-foreground 'helm-M-x-short-doc (plist-get base16-stylix-theme-colors :base0F))
  (set-face-foreground 'helm-M-x-key (plist-get base16-stylix-theme-colors :base0F)))

;; --------------------------- Fonts and font locking --------------------------
(set-face-attribute 'default nil :height 185)

(custom-set-faces
 '(font-lock-comment-face
   ((t (:family "Liberation Serif" :italic t :height 220))))
 '(font-lock-doc-face
   ((t (:family "Liberation Serif" :italic t :height 220))))
 '(font-lock-string-face  ((t (:italic t)))))

(font-lock-add-keywords
 'emacs-lisp-mode
 '(("#'" . 'font-lock-function-call-face)))

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
  :hook (prog-mode-hook . rainbow-delimiters-mode))

(use-package
  rainbow-identifiers
  :hook (prog-mode-hook . rainbow-identifiers-mode))

(use-package
  highlight-defined
  :hook (prog-mode-hook . highlight-defined-mode))

(use-package
  highlight-numbers
  :hook (prog-mode-hook . highlight-numbers-mode))

(use-package
  diredfl
  :hook (dired-mode . diredfl-mode))

(with-eval-after-load
  'tex
  (set-face-foreground 'font-latex-math-face (plist-get base16-stylix-theme-colors :base04)))

;; --------------------------------- Ultrawide ---------------------------------
(use-package
  olivetti
  :hook ((prog-mode-hook . olivetti-mode)
	 (helm-major-mode-hook . olivetti-mode)
	 (Man-mode-hook . olivetti-mode)
	 (LaTeX-mode-hook . olivetti-mode))
  :config
  (setq-default olivetti-body-width 130))

;; ------------------------------ Smooth scrolling -----------------------------
(use-package
  smooth-scroll
  :config
  (setq smooth-scroll/vscroll-step-size 1)
  (smooth-scroll-mode))

(provide 'theme)
