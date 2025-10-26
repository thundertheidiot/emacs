;; -*- lexical-binding: t; -*-
(require 'meow-helpers)

(setq backup-directory-alist
      `(("." . ,(expand-file-name "backup-files" user-emacs-directory))))

(let ((auto-save-dir (expand-file-name "auto-saves/" user-emacs-directory)))
  (setq auto-save-list-file-prefix auto-save-dir
	auto-save-file-name-transforms `((".*" ,auto-save-dir t)))
  (add-to-list 'meow/create-directories auto-save-dir))

(setq create-lockfiles nil)

(provide 'meow-cleanup)
