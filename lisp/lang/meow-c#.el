;; -*- lexical-binding: t; -*-
(defun tmc-test ()
  (interactive)
  (let* ((test-dir (expand-file-name "../.." default-directory))
	 (default-directory test-dir))
    (async-shell-command "tmc test")))

(defun tmc-submit ()
  (interactive)
  (let* ((test-dir (expand-file-name "../.." default-directory))
	 (default-directory test-dir))
    (async-shell-command "tmc submit")))

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
    "t" #'tmc-test
    "s" #'tmc-submit
    "r" #'dotnet-run))

(provide 'lang/meow-c\#)
