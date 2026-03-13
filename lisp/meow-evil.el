;; -*- lexical-binding: t; -*-
(require 'meow-undo)
(require 'undo-tree)

(setq evil-want-keybinding nil)

(use-package evil
  :custom
  (evil-want-integration t)
  (evil-want-minibuffer t)
  (evil-vsplit-window-right t)
  (evil-split-window-below t)
  (evil-undo-system 'undo-tree)
  :config
  (evil-set-undo-system evil-undo-system)
  (evil-set-initial-state 'minibuffer-mode 'insert)
  (evil-set-initial-state 'minibuffer-inactive-mode 'insert)
  (evil-mode))

(use-package evil-collection
  :after evil
  :custom
  (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init '(apropos
						  calc
						  compile
						  comint
						  dashboard
						  debug
						  ediff
						  emoji
						  eshell
						  woman
						  pdf
						  org
						  proced
						  dired
						  elfeed
						  wdired
						  image
						  ibuffer
						  simple-mpc
						  magit
						  magit-todos
						  vdiff
						  sly
						  wgrep
						  yaml-mode
						  diff-hl
						  vterm
						  eat)))

(use-package evil-better-visual-line
  :demand t
  :after evil
  :config
  (evil-better-visual-line-on))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(provide 'meow-evil)
