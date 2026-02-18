;; -*- lexical-binding: t; -*-
(defun dotnet-run ()
  (interactive)
  (async-shell-command "dotnet run"))

(use-package csharp-mode
  :ensure nil
  :mode "\\.cs\\'"
  :hook
  (csharp-mode . eglot-ensure)
  (csharp-mode . apheleia-mode)
  :general-config
  (meow/local :keymaps 'csharp-mode-map
    "t" (lambda () (interactive) (meow/tmc "test"))
    "s" (lambda () (interactive) (meow/tmc "submit"))
    "r" #'dotnet-run))

(provide 'lang/meow-c\#)
