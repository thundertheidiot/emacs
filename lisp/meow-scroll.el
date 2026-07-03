;; -*- lexical-binding: t; -*-

;; better pixel scrolling
(use-package ultra-scroll
  :init
  (setq scroll-conservatively 101
        scroll-margin 0
		jit-lock-defer-time 0.05)  ; defer fontification
  :config
  (add-hook 'ultra-scroll-hide-functions #'jit-lock-mode)
  (ultra-scroll-mode 1))

(provide 'meow-scroll)
;;; meow-scroll.el ends here
