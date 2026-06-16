;; -*- lexical-binding: t; -*-

(use-package eglot
  :commands eglot-ensure
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)
  (eglot-sync-connect nil)
  (eglot-connect-timeout nil)
  :config
  (fset #'jsonrpc--log-event #'ignore)
  :general-config
  (meow/leader
    "c" '(:ignore t :wk "code")
    "ca" '("code actions" . (lambda () (interactive)
							  (eglot-code-actions (point-min) (point-max) nil t))))
  (:states '(normal visual insert)
		   "M-r" #'eglot-rename))

(require 'eglot-booster)
(eglot-booster-mode)

;; lsp mode for the couple things that need it, vue rn
(use-package lsp-mode
  :custom
  (lsp-log-io nil)
  (lsp-headerline-breadcrumb-enable nil)
  :config
  (require 'lsp-mode-autoloads)
  (require 'lsp-javascript)
  (require 'lsp-volar)
  (require 'lsp-tailwindcss))

;; (use-package lsp-ui
;;   :hook (lsp-mode . lsp-ui-mode)
;;   :custom
;;   )

(provide 'meow-lsp)
;;; meow-lsp.el ends here
