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

(add-hook 'post-command-hook
          (lambda ()
            (+set-default-frame-parameters
              'right-divider-width 2
              'bottom-divider-width 2
              'internal-border-width 0)
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
      (lambda (v) (- v 0.1)))))

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
   ((t (:family "Liberation Serif" :italic t :height 215))))
 '(font-lock-doc-face
   ((t (:family "Liberation Serif" :italic t :height 215))))
 '(font-lock-string-face  ((t (:italic t)))))

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
  'font-latex
  (set-face-foreground 'font-latex-math-face (plist-get base16-stylix-theme-colors :base04)))

;; --------------------------------- Ultrawide ---------------------------------
(use-package
  olivetti
  :hook ((prog-mode-hook . olivetti-mode)
	 (helm-major-mode-hook . olivetti-mode)
	 (Man-mode-hook . olivetti-mode)
	 (dired-mode-hook . olivetti-mode)
	 (org-mode-hook . olivetti-mode)
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
