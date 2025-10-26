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

(use-package nix-mode
  :mode "\\.nix\\'"
  :hook (nix-mode . eglot-ensure)
  :commands (meow/nix-repl)
  :general
  (meow/leader
    "on" '("nix repl" . meow/nix-repl)
    "pon" '("nix repl" . (lambda () (interactive)
			   (let ((default-directory (projectile-project-root)))
			     (meow/nix-repl))))
    )
  :config
  (require 'nix-repl))


(provide 'lang/meow-nix)
;;; meow-nix.el ends here
