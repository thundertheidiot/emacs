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
  ;; nixos executable is OmniSharp
  (setf
   (alist-get '(csharp-mode csharp-ts-mode)
	      eglot-server-programs nil nil #'equal) '("OmniSharp" "-lsp"))
  :general-config
  (meow/leader
    "c" '(:ignore t :wk "code")
    "ca" '("code actions" . (lambda () (interactive)
			      (eglot-code-actions (point-min) (point-max) nil t))))
  (:states '(normal visual insert)
	   "M-r" #'eglot-rename))

(use-package eglot-booster
  :after eglot
  :config (eglot-booster-mode))

(provide 'meow-lsp)
;;; meow-lsp.el ends here
