;; -*- lexical-binding: t; -*-
(require 'meow-helpers)

(setq create-lockfiles nil)

(unless (string= (getenv "HOME") "/homeless-shelter")
  (use-package no-littering)
  (no-littering-theme-backups))

(provide 'meow-cleanup)
;;; meow-cleanup.el ends here
