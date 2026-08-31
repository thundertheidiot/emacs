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
  ;; this fixes keybind conflicts
  (setq evil-collection-binding-overrides
        `((repl-submit :state insert)
		  (repl-newline :state normal :enabled
						(lambda (map-sym &rest _)
						  (not (eq map-sym 'eshell-mode-map))))
		  (repl-force-newline :enabled nil)))
  (evil-collection-init '(apropos
						  arc-mode
						  calc
						  comint
						  compile
						  dashboard
						  debug
						  diff-hl
						  dired
						  eat
						  ediff
						  elfeed
						  emoji
						  eshell
						  ibuffer
						  image
						  magit
						  magit-todos
						  org
						  pdf
						  proced
						  replace
						  simple-mpc
						  sly
						  vdiff
						  vterm
						  wdired
						  wgrep
						  woman
						  yaml-mode)))

(use-package evil-better-visual-line
  :demand t
  :after evil
  :config
  (evil-better-visual-line-on))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :config
  (evil-commentary-mode))

(use-package evil-matchit
  :config
  (global-evil-matchit-mode 1))

(defun meow/evil-replace-string (string)
  (interactive
   (when (region-active-p)
	 (list (buffer-substring-no-properties (region-beginning) (region-end)))))
  (run-at-time 0 nil ;; evil-ex blocks, this way we call `evil--ex-update' after the minibuffer is created
			   (lambda ()
				 (when (minibufferp)
				   (evil--ex-update (point-min)))))
  (evil-ex (format "%%s/%s" string)))

(provide 'meow-evil)
