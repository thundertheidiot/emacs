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

(provide 'meow/programming)
;;; programming.el ends here
