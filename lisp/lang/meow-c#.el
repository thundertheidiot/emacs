;; -*- lexical-binding: t; -*-
(defun meow/dotnet-run ()
  (interactive)
  (async-shell-command "dotnet run"
		       (get-buffer-create "*dotnet run*")))

(use-package csharp-mode
  :demand t
  :ensure nil
  :mode "\\.cs\\'"
  :config
  ;; nixos executable is OmniSharp
  (setf
   (alist-get '(csharp-mode csharp-ts-mode)
	      eglot-server-programs nil nil #'equal) '("OmniSharp" "-lsp"))
  :hook
  (csharp-mode . eglot-ensure)
  (csharp-mode . apheleia-mode)
  :general-config
  (meow/local :keymaps 'csharp-mode-map
    "t" (lambda () (interactive) (meow/tmc "test"))
    "s" (lambda () (interactive) (meow/tmc "submit"))
    "r" #'meow/dotnet-run))

(provide 'lang/meow-c\#)
