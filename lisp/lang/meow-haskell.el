;;; -*- lexical-binding: t; -*-
(defun meow/--ghci-buffer ()
  (get-buffer-create (format "*ghci %s*" default-directory)))

(defvar-local meow/ghci-buffer nil)
(defvar-local meow/ghci-files '())

(defun meow/associate-ghci-buffer (buffer)
  (interactive (list (read-buffer "GHCI Buffer: " nil t
								  (lambda (buf)
									(with-current-buffer (if (consp buf) (cdr buf) buf)
									  (eq major-mode 'inferior-haskell-mode))))))
  (setq meow/ghci-buffer buffer)
  (let ((file-name (expand-file-name buffer-file-name)))
	(with-current-buffer buffer
	  (unless (seq-contains-p meow/ghci-files file-name)
		(setq meow/ghci-files (append meow/ghci-files (list file-name))))
	  (comint-send-string (get-buffer-process meow/ghci-buffer)
						  (format ":load %s\n"
								  (mapconcat #'identity meow/ghci-files " ")))))
  (add-hook 'after-save-hook (lambda ()
							   (when meow/ghci-buffer
								 (comint-send-string (get-buffer-process meow/ghci-buffer)
													 ":reload\n")))
			nil t))

(defun meow/ghci-associate-haskell-buffer (buffer)
  (interactive (list (read-buffer "Haskell Buffer: " nil t
								  (lambda (buf)
									(with-current-buffer (if (consp buf) (cdr buf) buf)
									  (eq major-mode 'haskell-mode))))))
  (let ((ghci (current-buffer)))
	(with-current-buffer buffer
	  (meow/associate-ghci-buffer ghci))))

(defun meow/ghci (&optional no-split)
  (interactive)
  (unless (and (featurep 'haskell-mode) (featurep 'inf-haskell))
	(mapc #'require '(haskell-mode inf-haskell)))
  (let ((current-buffer (current-buffer)))
	(unless no-split
	  (select-window (meow/intelligent-split t)))
	(let ((buffer (meow/--ghci-buffer))
		  (command (haskell-program-name-with-args)))
	  (unless (comint-check-proc buffer)
		(with-current-buffer buffer
		  (apply 'make-comint-in-buffer "haskell" buffer (car command) nil (cdr command))
		  (inferior-haskell-mode)))
	  (meow/associate-ghci-buffer buffer)
	  (pop-to-buffer-same-window buffer))))

(use-package haskell-mode
  :mode "\\.hs\\'"
  :hook (haskell-mode . eglot-ensure)
  :custom
  (haskell-process-args-ghci '("-ferror-spans"
							   "-fdiagnostics-color=always"
							   "-fprint-error-index-links=always"
							   "-fprint-expanded-synonyms"))
  :general-config
  (meow/local :keymaps 'haskell-mode-map
	"h" #'meow/ghci
	"H" (lambda () (interactive) (meow/ghci t))
	"g" #'meow/associate-ghci-buffer)
  (meow/local :keymaps 'inferior-haskell-mode-map
	"g" #'meow/ghci-associate-haskell-buffer)
  :config
  (require 'haskell-mode-autoloads)
  (require 'inf-haskell))

(provide 'lang/meow-haskell)
;;; meow-haskell.el ends here
