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
(defvar-local meow/nix-build-binpath nil)

(defun meow/nix-build-and-run ()
  "Build the current file with nix, run an executable."
  (interactive)
  (let* ((buffer (get-buffer-create (format "*nix build&run %s*"
					    buffer-file-name)))
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
		     (bin (or (ignore-errors (expand-file-name meow/nix-build-binpath path))
			      (read-file-name "Select executable: "
					      path nil t nil
					      #'file-executable-p))))
		(async-shell-command bin buffer))))))
    (async-shell-command
     (format "nix build --impure --print-out-paths --expr '%s'"
	     (or meow/nix-build-expression
		 (format
		  "with import <nixpkgs> {}; callPackage \"%s\" %s"
		  buffer-file-name
		  meow/nix-build-callpackage-expression)
		 ))
     buffer)
    (set-process-sentinel (get-buffer-process buffer) sentinel)
    ))

(use-package nix-mode
  :demand t ;; lazy loading is bad, i am an emacs server user
  :mode "\\.nix\\'"
  :hook (nix-mode . eglot-ensure)
  :commands (meow/nix-repl)
  :general-config
  (meow/leader
    "on" '("nix repl" . meow/nix-repl)
    "oN" '("nix repl" . (lambda () (interactive)
			  (meow/nix-repl t)))
    "pon" '("nix repl" . (lambda () (interactive)
			   (let ((default-directory (projectile-project-root)))
			     (meow/nix-repl))))
    "poN" '("nix repl" . (lambda () (interactive)
			   (let ((default-directory (projectile-project-root)))
			     (meow/nix-repl t)))))
  (meow/local :keymaps 'nix-mode-map
    "b" '("nix build" . meow/nix-build-and-run))
  :config
  (require 'nix-repl))


(provide 'lang/meow-nix)
;;; meow-nix.el ends here
