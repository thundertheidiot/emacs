;; -*- lexical-binding: t; -*-
(require 'meow-helpers)
(require 'meow-mode-line)

(require 'diff-hl)
(require 'olivetti)

;; different background color for "unimportant" frames
(use-package solaire-mode
  :config
  (meow/runonce
   "solaire" nil
   (when (display-graphic-p)
     (solaire-global-mode 1))))

(use-package catppuccin-theme
  :after solaire-mode
  :custom
  (catppuccin-flavor 'mocha)
  :config
  ;; https://github.com/catppuccin/emacs/issues/121
  (meow/runonce
   "catppuccin-121" t
   (catppuccin-reload))

  (load-theme 'catppuccin :no-confirm)
  (meow/mode-line))

(let ((green (face-attribute 'success :foreground))
      (purple (face-attribute 'font-lock-keyword-face :foreground))
      (red (face-attribute 'error :foreground)))
  (mapc
   (lambda (f)
     (set-face-attribute f nil
						 :background green
						 :foreground green))
   '(diff-hl-insert diff-hl-dired-insert diff-hl-margin-insert))
  (mapc
   (lambda (f)
     (set-face-attribute f nil
						 :background purple
						 :foreground purple))
   '(diff-hl-change diff-hl-dired-change diff-hl-margin-change))
  (mapc
   (lambda (f)
     (set-face-attribute f nil
						 :background red
						 :foreground red))
   '(diff-hl-delete diff-hl-dired-delete diff-hl-margin-delete)))

(set-face-attribute 'olivetti-fringe nil :inherit 'solaire-fringe-face)

(add-to-list 'default-frame-alist '(font . "Monospace"))

;; font setup
(meow/runonce
 "fonts" nil
 (set-face-attribute 'default nil
					 :family "Monospace"
					 :height 110
					 :weight 'regular)

 (set-face-attribute 'variable-pitch nil
					 :font "Sans-Serif"
					 :height 120
					 :weight 'medium)

 (dolist (face '(minibuffer-prompt))
   (set-face-attribute face nil
					   :height 1.1))

 (set-face-attribute 'fixed-pitch nil
					 :font "Monospace"
					 :weight 'medium)

 (set-face-attribute 'font-lock-comment-face nil
					 :slant 'italic)
 (set-face-attribute 'font-lock-keyword-face nil
					 :slant 'italic))

(add-hook 'minibuffer-setup-hook
		  (lambda ()
			(text-scale-set 1.1)))

(dolist (buf '(" *Echo Area 0*" " *Echo Area 1*"))
  (with-current-buffer (get-buffer-create buf)
    (text-scale-set 1.1)))
(setq resize-mini-windows 'grow-only
      max-mini-window-height 2)

(setq-default line-spacing 0.12)

(provide 'meow-theme)
