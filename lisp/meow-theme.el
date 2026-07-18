;; -*- lexical-binding: t; -*-
(require 'meow-helpers)
(require 'meow-mode-line)

(require 'olivetti)
(require 'lsp-mode)

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
  (meow/runonce
   "solaire" nil
   (when (display-graphic-p)
     (solaire-global-mode 1))))

(use-package batppuccin
  :config
  (require 'batppuccin-autoloads)
  (load-theme 'batppuccin-mocha t)
  (meow/mode-line)

  (meow/setfaces
   ;; take height from default so decrease/increase font size works
   'line-number `(:inherit default)
   'line-number-current-line `(:inherit default)

   'consult-highlight-match `(:foreground ,(batppuccin-get-color "bat-green") :underline t :weight bold :background nil)
   'match `(:foreground ,(batppuccin-get-color "bat-green") :underline t :weight bold :background ,(batppuccin-get-color "bat-surface1"))
   
   'evil-ex-substitute-matches `(:foreground ,(batppuccin-get-color "bat-red") :underline t :weight bold :background nil)
   'evil-ex-substitute-replacement `(:foreground ,(batppuccin-get-color "bat-green") :underline t)

   'web-mode-block-delimiter-face `(:foreground ,(batppuccin-get-color "bat-yellow"))
   'rainbow-delimiters-unmatched-face `(:box nil)))

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

(set-face-attribute 'olivetti-fringe nil :inherit 'solaire-fringe-face)

(add-to-list 'default-frame-alist '(font . "Monospace-14"))
(set-frame-font "Monospace-14")

(add-hook 'server-after-make-frame-hook
		  (lambda ()
			(set-frame-font "Monospace-14")))

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
