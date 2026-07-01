;; -*- lexical-binding: t; -*-

(use-package eglot
  :commands eglot-ensure
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)
  (eglot-sync-connect nil)
  (eglot-connect-timeout nil)
  :config
  (fset #'jsonrpc--log-event #'ignore))

(require 'eglot-booster)
(setq eglot-booster-io-only t)
(eglot-booster-mode)

;; lsp mode for the couple things that need it, vue rn
(use-package lsp-mode
  :demand t
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

  ;; allow filtering completions for lsp capf
  (add-to-list 'completion-category-overrides
			   '(lsp-capf (styles hotfuzz orderless basic)))

  ;; handled by apheleia
  (setq lsp-typescript-format-enable nil
		lsp-javascript-format-enable nil
		lsp-enable-on-type-formatting nil
		lsp-enable-indentation nil
		lsp-enable-relative-indentation nil
		lsp-format-buffer-on-save nil)

  ;; according to claude this may fix freezes 🤷‍♀️
  (setq lsp-semantic-tokens-enable nil))

(defun meow/lsp-supercomplete ()
  "Set up fast autocompletion based on dabbrev and lsp completion."
  (interactive)
  (setq-local completion-at-point-functions
			  (list
			   #'cape-file
			   (cape-capf-super
				;; (cape-capf-prefix-length #'lsp-completion-at-point 3)
				#'lsp-completion-at-point
				#'cape-dabbrev)
			   #'cape-keyword)
			  corfu-auto-delay 0.01
			  corfu-auto-prefix 1))

(provide 'meow-lsp)
;;; meow-lsp.el ends here
