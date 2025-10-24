;; -*- lexical-binding: t; -*-
(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :custom
  (evil-want-keybinding nil)
  (evil-want-integration t)
  (evil-want-minibuffer t)
  (evil-vsplit-window-right t)
  (evil-split-window-below t)
  (evil-undo-system 'undo-tree)
  :config
  (evil-set-undo-system evil-undo-system)
  (evil-mode))

(use-package evil-collection
  :after evil
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
			  forge
			  magit-todos
			  vdiff
			  sly
			  wgrep
			  yaml-mode
			  diff-hl
			  vterm)))

(use-package evil-better-visual-line
  :demand t
  :after evil
  :config
  (evil-better-visual-line-on))

(use-package general
  :demand t
  :config
  (general-evil-setup))

(general-create-definer meow/leader
  :states '(normal insert visual emacs motion)
  :keymaps 'override
  :prefix "SPC"
  :global-prefix "C-SPC")

(general-create-definer meow/local
  :states '(normal insert visual emacs motion)
  :prefix "SPC l"
  :global-prefix "C-SPC l")

;; basic keybindings
(meow/leader
  "." '("find file" . find-file))

(provide 'meow/keybindings)
;;; keybindings.el ends here
