;; -*- lexical-binding: t; -*-
(require 'meow-helpers)

(defun meow/nix-repl (&optional no-split)
  "Load the Nix-REPL.  Set NO-SPLIT to not split a new window."
  (interactive "P")
  (unless (and (featurep 'nix-mode) (featurep 'nix-repl))
    (mapc #'require '(nix-mode nix-repl)))
  (unless no-split
    (select-window (meow/intelligent-split t)))
  (let ((nix-repl-executable-args
		 (if (file-exists-p (expand-file-name "flake.nix" default-directory))
			 `("repl"
			   "--expr"
			   ,(format "builtins.getFlake \"%s\"" (expand-file-name default-directory)))
		   nix-repl-executable-args)))
    (pop-to-buffer-same-window (generate-new-buffer "*nix-repl*"))
    (nix--make-repl-in-buffer (current-buffer))
    (nix-repl-mode)))

(defvar-local meow/nix-build-callpackage-expression "{}")
(defvar-local meow/nix-build-expression nil)
(defvar-local meow/nix-build-binpath nil
  "Executable path relative to the build output.")

(defun meow/nix-build (&optional buffer)
  "Build the current file with nix build, using a callPackage expression."
  (interactive)
  (async-shell-command
   (format "nix build --print-build-logs --impure --print-out-paths --expr %s"
		   (shell-quote-argument
			(or meow/nix-build-expression
				(format
				 "with import <nixpkgs> {}; callPackage \"%s\" %s"
				 (replace-regexp-in-string
				  "[\\\\\"]\\|\\${" (lambda (match) (concat "\\" match))
				  buffer-file-name t t)
				 meow/nix-build-callpackage-expression))))
   (or buffer
       (get-buffer-create "*nix build*"))))

(defun meow/nix-build-and-run (&optional arg)
  "Build the current file with nix, run an executable.
With prefix ARG, select an executable again."
  (interactive "P")
  (let* ((buffer (get-buffer-create (format "*nix build&run %s*"
											buffer-file-name)))
		 (current-buffer (current-buffer))
		 (sentinel
		  (lambda (process _signal)
			(when (and
				   (memq (process-status process) '(exit signal))
				   (eq (process-exit-status process) 0))
			  (let* ((path
					  (with-current-buffer buffer
						(goto-char (point-max))
						(skip-chars-backward "\n\t ")
						(buffer-substring (line-beginning-position) (line-end-position))))
					 (bin (with-current-buffer current-buffer
							(expand-file-name
							 (if (and (not arg) meow/nix-build-binpath)
								 meow/nix-build-binpath
							   (setq-local meow/nix-build-binpath
										   (file-relative-name
											(expand-file-name
											 (read-file-name "Select executable: "
															 path nil t nil
															 #'file-executable-p)
											 path)
											path)))
							 path))))
				(async-shell-command (shell-quote-argument bin) buffer))))))
    (meow/nix-build buffer)
    (set-process-sentinel (get-buffer-process buffer) sentinel)))

(defun meow/nix-run ()
  "Simple nix run."
  (interactive)
  (async-shell-command "nix run --print-build-logs"
					   (get-buffer-create "*nix run*")))

(use-package nix-mode
  :demand t ;; lazy loading is bad, i am an emacs server user
  :mode "\\.nix\\'"
  :hook (nix-mode . lsp-deferred)
  :commands (meow/nix-repl)
  :general-config
  (meow/leader
    "nr" '("nix run" . meow/nix-run)
    "on" '("nix repl" . meow/nix-repl)
    "oN" '("nix repl" . (lambda () (interactive)
						  (meow/nix-repl t)))
    "pon" '("nix repl" . (lambda () (interactive)
						   (let ((default-directory (project-root (project-current))))
							 (meow/nix-repl))))
    "poN" '("nix repl" . (lambda () (interactive)
						   (let ((default-directory (project-root (project-current))))
							 (meow/nix-repl t)))))
  (meow/local :keymaps 'nix-mode-map
    "r" '("nix build&run" . meow/nix-build-and-run)
    "b" '("nix build" . meow/nix-build))
  :config
  (require 'nix-repl))


(provide 'lang/meow-nix)
;;; meow-nix.el ends here
