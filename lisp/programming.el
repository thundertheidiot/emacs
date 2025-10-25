;; -*- lexical-binding: t; -*-
(use-package flycheck
  :hook
  (prog-mode . flycheck-mode)
  (eglot-managed-mode . flycheck-mode)
  (flycheck-mode . (lambda () (flycheck-set-indication-mode 'left-fringe)))
  :general-config
  (meow/leader
    "cn" '("next error" . flycheck-next-error)
    "cN" '("previous error" . flycheck-previous-error)))

(use-package consult-flycheck
  :general
  (:states '(normal visual motion) :keymaps 'override :prefix "SPC"
	   "sd" '("flycheck" . consult-flycheck)
	   ))

(use-package flycheck-eglot
  :demand t
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))

(use-package apheleia
  :demand t
  :diminish apheleia
  :config
  (setf (alist-get 'nixfmt apheleia-formatters)
	'("alejandra"))
  (apheleia-global-mode +1))

(use-package projectile
  :diminish projectile-mode
  :commands (projectile-run-eshell projectile-run-vterm)
  :custom
  (projectile-switch-project-action #'projectile-dired)
  :config
  (projectile-mode)
  :general
  (:states '(normal visual motion) :keymaps 'override :prefix "SPC"
	   "P" '(:keymap projectile-command-map :package projectile)
	   "p" '(:ignore t :package projectile :wk "project")
	   "pp" '("switch project" . projectile-switch-project)
	   "ps" '("search project" . (lambda () (interactive) (consult-ripgrep (projectile-project-root))))
	   "p." '("find project file" . projectile-find-file)
	   "po" '(:ignore t :wk "open")
	   "pog" '("project version control (git)" . projectile-vc)
	   "pb" '("switch buffer in project" . projectile-switch-to-buffer)))

(use-package ibuffer-projectile
  :hook
  (ibuffer-mode . (lambda () (ibuffer-projectile-set-filter-groups)
		    (unless (eq ibuffer-sorting-mode 'alphabetic)
		      (ibuffer-do-sort-by-alphabetic)))))

(use-package hl-todo
  :demand t
  :diminish hl-todo-mode
  :diminish global-hl-todo-mode
  :custom
  (hl-todo-keyword-faces '(("TODO" . ,(face-attribute 'error :foreground))
			   ("HACK" . ,(face-attribute 'warning :foreground))
			   ("NOTE" . ,(face-attribute 'match :foreground))
			   ("FIXME" . ,(face-attribute 'error :foreground))))
  :config
  (global-hl-todo-mode 1))

;; (use-package yasnippet
;;   :custom
;;   (yas-snippets-dirs (expand-file-name "snippets" user-emacs-directory))
;;   :config
;;   (yas-global-mode 1))

(use-package envrc
  :demand t
  :hook (after-init . envrc-global-mode))

(provide 'meow/programming)
;;; programming.el ends here
