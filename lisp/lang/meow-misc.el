;;; -*- lexical-binding: t; -*-

(defun meow/--ghci-buffer ()
  (get-buffer-create (format "*ghci %s*" default-directory)))

(defun meow/ghci (&optional no-split)
  (interactive)
  (unless (and (featurep 'haskell-mode) (featurep 'inf-haskell))
	(mapc #'require '(haskell-mode inf-haskell)))
  (unless no-split
	(select-window (meow/intelligent-split t)))
  (let ((buffer (meow/--ghci-buffer))
		(command (haskell-program-name-with-args)))
	(unless (comint-check-proc buffer)
	  (with-current-buffer buffer
		(apply 'make-comint-in-buffer "haskell" buffer (car command) nil (cdr command))
		(inferior-haskell-mode)
		(run-hooks 'inferior-haskell-hook)))
	(pop-to-buffer-same-window buffer)))

(use-package haskell-mode
  :mode "\\.hs\\'"
  :hook (haskell-mode . eglot-ensure)
  :custom
  (haskell-process-args-ghci '("-ferror-spans"
							   "-fdiagnostics-color=always"
							   "-fprint-error-index-links=always"
							   "-fprint-expanded-synonyms"
							   ))
  :general-config
  (meow/local :keymaps 'haskell-mode-map
	"h" #'meow/ghci
	"H" (lambda () (interactive) (meow/ghci t)))
  :config
  (require 'haskell-mode-autoloads))

(use-package lua-mode
  :mode "\\.lua\\'"
  :hook (lua-mode . eglot-ensure))

(use-package glsl-mode)

(use-package just-mode)

(use-package sly
  :custom
  (inferior-lisp-program "sbcl"))

(use-package janet-mode
  :mode "\\.janet\\'")

(add-hook 'emacs-lisp-mode-hook #'corfu-mode)

(use-package qml-mode
  :mode "\\.qml\\'")

(provide 'lang/meow-misc)
;;; meow-misc.el ends here
