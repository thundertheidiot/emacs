;; -*- lexical-binding: t; -*-
(defun meow/--cargo-toml-edition (toml)
  "Return the edition declared in Cargo.toml file TOML."
  (with-temp-buffer
	(insert-file-contents toml)
	(let ((section nil) (edition nil))
	  (while (and (not edition) (not (eobp)))
		(cond
		 ((looking-at "^[ \t]*\\[\\([^]]+\\)\\]")
		  (setq section (string-trim (match-string 1))))
		 ((and (member section '("package" "workspace.package"))
			   (looking-at "^[ \t]*edition[ \t]*=[ \t]*\"\\([0-9]+\\)\""))
		  (setq edition (match-string 1)))
		 ((and (equal section "package")
			   (looking-at "^[ \t]*edition\\(?:[ \t]*\\.[ \t]*workspace[ \t]*=\\|[ \t]*=[ \t]*{[^}]*workspace\\)"))
		  (setq edition 'workspace)))
		(forward-line 1))
	  edition)))

(defun meow/rust-edition ()
  "Edition `cargo fmt' would pass to rustfmt for the current buffer.
Follows `edition.workspace = true' up to the workspace root."
  (let ((dir default-directory) (result nil) (in-crate nil))
	(while (and dir (not result))
	  (if-let* ((root (locate-dominating-file dir "Cargo.toml")))
		  (let ((edition (meow/--cargo-toml-edition
						  (expand-file-name "Cargo.toml" root))))
			(setq in-crate t)
			(if (stringp edition)
				(setq result edition)
			  (setq dir (and (eq edition 'workspace)
							 (file-name-directory
							  (directory-file-name (file-name-as-directory root))))))
			(unless (or result dir) (setq result "2015")))
		(setq dir nil)))
	(or result (if in-crate "2015" "2024"))))

(use-package rustic
  :mode ("\\.rs\\'" . rustic-mode)
  :hook (rustic-mode . eglot-ensure)
  :init
  (setq rustic-lsp-client 'eglot
		rustic-use-rust-save-some-buffers t
		compilation-ask-about-save nil)
  (setf (alist-get 'rustfmt apheleia-formatters)
		(list "rustfmt" "--quiet" "--edition" '(meow/rust-edition) "--emit" "stdout")))

(provide 'lang/meow-rust)
;;; meow-rust.el ends here
