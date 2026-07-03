;; -*- lexical-binding: t; -*-
(use-package rustic
  :mode ("\\.rs\\'" . rustic-mode)
  :hook (rustic-mode . lsp-deferred)
  :init
  (setq rustic-lsp-client 'lsp-mode
		rustic-use-rust-save-some-buffers t
		compilation-ask-about-save nil))

(provide 'lang/meow-rust)
;;; meow-rust.el ends here
