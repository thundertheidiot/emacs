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
							  (eglot-code-actions (point-min) (point-max) nil t)))))

(require 'eglot-booster)
(setq eglot-booster-io-only t)
(eglot-booster-mode)

;; lsp mode for the couple things that need it, vue rn
(use-package lsp-mode
  :init
  (setq lsp-use-plists t)
  :custom
  (lsp-log-io nil)
  (lsp-headerline-breadcrumb-enable nil)
  :config
  (require 'lsp-mode-autoloads)
  (require 'lsp-javascript)
  (require 'lsp-volar)
  (require 'lsp-tailwindcss)

  ;; handled by apheleia
  (setq lsp-typescript-format-enable nil
		lsp-javascript-format-enable nil
		lsp-enable-on-type-formatting nil
		lsp-enable-indentation nil
		lsp-enable-relative-indentation nil
		lsp-format-buffer-on-save nil))

(provide 'meow-lsp)
;;; meow-lsp.el ends here
