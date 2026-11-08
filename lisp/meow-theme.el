;; -*- lexical-binding: t; -*-
(require 'meow-helpers)
(require 'meow-mode-line)

(require 'olivetti)
(require 'lsp-mode)
(require 'ghostel)
(require 'consult)
(require 'evil-ex)
(require 'web-mode)
(require 'nix-mode)

(defface meow-lisp-keyword-face
  '((t (:inherit font-lock-constant-face)))
  "Face for Lisp keyword symbols."
  :group 'font-lock-faces)

(defmacro meow/setfaces (&rest args)
  (let ((forms))
	(while args
	  (let ((face (pop args))
			(attrs (pop args)))
		(push `(apply #'set-face-attribute ,face nil ,attrs) forms)))
	`(progn ,@(nreverse forms))))

;; different background color for "unimportant" frames
(use-package solaire-mode
  :config
  (meow/runonce-graphical "solaire"
    (solaire-global-mode 1)))

(defun meow/theme-setup (_theme)
  "Customize faces dependent on the current theme colors.
Colors follow the active Madoromi palette."
  (when madoromi--current
	(meow/setfaces
	 'default `(:background ,(madoromi-get-color "kami"))
	 'fringe `(:inherit default :background unspecified)
	 'solaire-default-face `(:background ,(madoromi-get-color "hyoshi"))
	 'solaire-fringe-face `(:inherit solaire-default-face :background unspecified)
	 'olivetti-fringe `(:inherit default :background unspecified)
	 'ghostel-default `(:inherit solaire-default-face :background ,(madoromi-get-color "hyoshi"))
	 'window-divider `(:foreground ,(madoromi-get-color "chitsu"))
	 'window-divider-first-pixel `(:foreground ,(madoromi-get-color "chitsu"))
	 'window-divider-last-pixel `(:foreground ,(madoromi-get-color "chitsu"))
	 'variable-pitch `(:family "Inter Nerd Font")

	 ;; take height from default so decrease/increase font size works
	 'line-number `(:inherit default)
	 'line-number-current-line `(:inherit default)

	 'consult-highlight-match `(:foreground ,(madoromi-get-color "tokiwa") :underline t :weight bold :background nil)
	 'match `(:foreground ,(madoromi-get-color "tokiwa") :underline t :weight bold :background ,(madoromi-get-color "nijimi0"))

	 'evil-ex-substitute-matches `(:foreground ,(madoromi-get-color "beni") :underline t :weight bold :background nil)
	 'evil-ex-substitute-replacement `(:foreground ,(madoromi-get-color "tokiwa") :underline t)

	 'web-mode-block-delimiter-face `(:foreground ,(madoromi-get-color "sumi")))
	;; Keep ordinary code neutral; reserve accents for syntax landmarks.
	(dolist (face '(font-lock-variable-name-face font-lock-variable-use-face
												 font-lock-function-call-face font-lock-property-name-face
												 font-lock-property-use-face font-lock-operator-face
												 font-lock-bracket-face font-lock-delimiter-face
												 font-lock-punctuation-face font-lock-constant-face
												 font-lock-number-face font-lock-builtin-face))
	  (set-face-attribute face nil :foreground (madoromi-get-color "sumi")))
	(meow/setfaces
	 'meow-lisp-keyword-face `(:foreground ,(madoromi-get-color "beni"))
	 'font-lock-keyword-face `(:foreground ,(madoromi-get-color "botan"))
	 'font-lock-function-name-face `(:foreground ,(madoromi-get-color "ai"))
	 'font-lock-type-face `(:foreground ,(madoromi-get-color "ai"))
	 'font-lock-string-face `(:foreground ,(madoromi-get-color "tokiwa"))
	 'font-lock-comment-face `(:foreground ,(madoromi-get-color "usuzumi0"))
	 'font-lock-comment-delimiter-face `(:foreground ,(madoromi-get-color "usuzumi0"))
	 'font-lock-doc-face `(:foreground ,(madoromi-get-color "usuzumi0")))
	;; Nix uses constants for paths and builtins for import and literals.
	(meow/setfaces
	 'nix-constant-face `(:foreground ,(madoromi-get-color "kaki"))
	 'nix-builtin-face `(:foreground ,(madoromi-get-color "botan"))
	 'nix-attribute-face `(:foreground ,(madoromi-get-color "ai")))
	(ghostel-sync-theme))
  

  (setopt hl-todo-keyword-faces `(("TODO" . ,(face-attribute 'error :foreground))
								  ("HACK" . ,(face-attribute 'warning :foreground))
								  ("NOTE" . ,(face-attribute 'match :foreground))
								  ("FIXME" . ,(face-attribute 'error :foreground)))))

(use-package madoromi
  :config
  (add-to-list 'custom-theme-load-path
               (file-name-directory (locate-library "madoromi-aki-hiru-theme")))
  (load-theme 'madoromi-aki-hiru t)

  (meow/theme-setup nil)

  (remove-hook 'enable-theme-functions #'meow/theme-setup)
  (add-hook 'enable-theme-functions #'meow/theme-setup 90)
  (meow/mode-line)
  (meow/runonce-graphical "mode-line"
    (meow/mode-line)))

;; (let ((green (face-attribute 'success :foreground))
;;       (purple (face-attribute 'font-lock-keyword-face :foreground))
;;       (red (face-attribute 'error :foreground)))
;;   (mapc
;;    (lambda (f)
;;      (set-face-attribute f nil
;; 						 :background green
;; 						 :foreground green))
;;    '(diff-hl-insert diff-hl-dired-insert diff-hl-margin-insert))
;;   (mapc
;;    (lambda (f)
;;      (set-face-attribute f nil
;; 						 :background purple
;; 						 :foreground purple))
;;    '(diff-hl-change diff-hl-dired-change diff-hl-margin-change))
;;   (mapc
;;    (lambda (f)
;;      (set-face-attribute f nil
;; 						 :background red
;; 						 :foreground red))
;;    '(diff-hl-delete diff-hl-dired-delete diff-hl-margin-delete)))

(dolist (parameter '((internal-border-width . 0)
                     (left-fringe . 10)
                     (right-fringe . 10)))
  (add-to-list 'default-frame-alist parameter)
  (when (display-graphic-p)
    (set-frame-parameter nil (car parameter) (cdr parameter))))

(setq window-divider-default-right-width 1
      window-divider-default-places 'right-only)
(window-divider-mode 1)

(add-to-list 'default-frame-alist '(font . "Monospace-14"))
(set-frame-font "Monospace-14")

(add-hook 'server-after-make-frame-hook
		  (lambda ()
			(set-frame-font "Monospace-14")
			(when (display-graphic-p)
			  (meow/mode-line))))

;; font setup
;; (meow/runonce
;;  "fonts" nil
;;  (set-face-attribute 'default nil
;; 					 :family "Monospace"
;; 					 :height 110
;; 					 :weight 'regular)

;;  (set-face-attribute 'variable-pitch nil
;; 					 :font "Sans-Serif"
;; 					 :height 120
;; 					 :weight 'medium)

;;  (dolist (face '(minibuffer-prompt))
;;    (set-face-attribute face nil
;; 					   :height 1.1))

;;  (set-face-attribute 'fixed-pitch nil
;; 					 :font "Monospace"
;; 					 :weight 'medium)

;;  (set-face-attribute 'font-lock-comment-face nil
;; 					 :slant 'italic)
;;  (set-face-attribute 'font-lock-keyword-face nil
;; 					 :slant 'italic))

;; (add-hook 'minibuffer-setup-hook
;; 		  (lambda ()
;; 			(text-scale-set 1.1)))

(setq resize-mini-windows 'grow-only
      max-mini-window-height 2)

(setq-default line-spacing 0.12)

(provide 'meow-theme)
