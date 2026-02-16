;; -*- lexical-binding: t; -*-
;; the best use of my time

(defun eshell-external-cargo-mommy (orig-fun command &optional &rest args)
  "Wrap eshell external commands with cargo-mommy.
This is an `:around' advice for `eshell-external-command'.

ORIG-FUN is the original function, COMMAND is the command.
ARGS is the list of arguments passed to the function."
  (if-let* ((mommy (executable-find "cargo-mommy"))
	    (_contains (not (member command eshell-visual-commands))))
      (let ((process-environment
	     (cons (format "CARGO_MOMMYS_ACTUAL=%s" command)
		   process-environment)))
	(apply orig-fun mommy args))
    (apply orig-fun command args)))

(advice-add 'eshell-external-command :around #'eshell-external-cargo-mommy)

(defun async-shell-command-cargo-mommy (orig-fun command &optional &rest args)
  (if-let* ((mommy (executable-find "cargo-mommy")))
      (let* ((split (split-string-shell-command command))
	     (process-environment
	      (cons (format "CARGO_MOMMYS_ACTUAL=%s" (car split))
		    process-environment))
	     (command (combine-and-quote-strings (cons mommy (cdr split)))))
	(apply orig-fun command args))
    (apply orig-fun command args)))

(advice-add 'async-shell-command :around #'async-shell-command-cargo-mommy)

(with-eval-after-load 'rustic
  (setq rustic-cargo-bin (or (executable-find "cargo-mommy") "cargo")))

(provide 'meow-mommy)
;;; meow-mommy.el ends here
