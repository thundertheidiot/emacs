;; -*- lexical-binding: t; -*-
(use-package rustic
  :mode ("\\.rs\\'" . rustic-mode)
  :hook (rustic-mode . eglot-ensure)
  :init
  (setq rustic-lsp-client 'eglot
		rustic-use-rust-save-some-buffers t
		compilation-ask-about-save nil)
  (setf (alist-get 'rustfmt apheleia-formatters)
		(list "rustfmt" "--quiet" "--edition" "2024" "--emit" "stdout")))

(provide 'lang/meow-rust)
;;; meow-rust.el ends here
