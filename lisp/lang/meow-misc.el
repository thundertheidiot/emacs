;;; -*- lexical-binding: t; -*-
(use-package haskell-mode
  :mode "\\.hs\\'"
  :hook (haskell-mode . eglot-ensure))

(use-package lua-mode
  :mode "\\.lua\\'"
  :hook (lua-mode . eglot-ensure))

(use-package gdscript-mode
  :mode "\\.gdscript\\'"
  :hook (gdscript-mode . eglot-ensure))

(use-package glsl-mode)

(add-hook 'emacs-lisp-mode-hook #'corfu-mode)

(use-package qml-mode
  :mode "\\.qml\\'")

(provide 'lang/meow-misc)
;;; meow-misc.el ends here
