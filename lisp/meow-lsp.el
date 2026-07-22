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

(use-package lsp-mode
  :demand t
  :init
  (setq lsp-use-plists t)

  (setq	lsp-enable-on-type-formatting nil
		lsp-enable-indentation nil
		lsp-enable-relative-indentation nil
		lsp-format-buffer-on-save nil)
  :custom
  (lsp-log-io nil)

  ;; slowness according to doom emacs
  (lsp-enable-folding nil)
  (lsp-enable-text-document-color nil)

  ;; formatting handled by apheleia
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-indentation nil)
  (lsp-enable-relative-indentation nil)
  (lsp-format-buffer-on-save nil)

  ;; disable headerline
  (lsp-headerline-breadcrumb-enable nil)

  ;; remove warning when company doesn't exist
  (lsp-completion-provider :none)
  
  ;; according to claude this may fix freezes 🤷‍♀️
  (lsp-semantic-tokens-enable nil)

  ;; use eldoc for info
  (lsp-eldoc-render-all t)

  ;; guess root
  (lsp-auto-guess-root t)
  :hook
  (lsp-mode . lsp-completion-mode)
  :config
  (require 'lsp-mode-autoloads)
  (require 'lsp-javascript)
  (require 'lsp-volar)
  (require 'lsp-tailwindcss)

  (setq lsp-typescript-preferences-import-module-specifier "non-relative"
		lsp-typescript-update-imports-on-file-move-enabled "prompt")

  ;; allow filtering completions for lsp capf
  (add-to-list 'completion-category-overrides
			   '(lsp-capf (styles hotfuzz orderless basic))))

(use-package yasnippet
  :custom
  (yas-snippets-dirs (expand-file-name "snippets" user-emacs-directory))
  :hook ((lsp-mode . yas-minor-mode)))


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
