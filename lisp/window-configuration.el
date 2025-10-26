;; -*- lexical-binding: t; -*-
(require 'subr-x) ;; shipped with emacs

(defvar meow/saved-window-configurations '()
  "Saved window configurations.")

(defun wcs--format-window-list ()
  (let ((window-list-string-formatted) (value))
    (dolist (window (window-list) value)
      (setq window-list-string-formatted (concat
					  window-list-string-formatted
					  (format "%s, " (buffer-name (window-buffer window))))))
    (setq window-list-string-formatted (string-remove-suffix ", " window-list-string-formatted))
    window-list-string-formatted))

(defun meow/save-window-configuration (&optional name)
  "Add the current window configuration to saved window configurations.
If NAME is provided, name it."
  (interactive "P")
  (when (and name (not (stringp name)))
    (setq name (completing-read "Name wcfg: " '())))
  (add-to-list 'meow/saved-window-configurations `(,(or name
							(if (string= (projectile-project-name) "-")
							    (format "%s (%s)"
								    (shell-command-to-string "date \"+%a %R\"")
								    (wcs--format-window-list))
							  (format "%s: %s (%s)"
								  (projectile-project-name)
								  (shell-command-to-string "date \"+%a %R\"")
								  (wcs--format-window-list))))
						   . ,(window-state-get (frame-root-window) t))))

(defun meow/new-window-configuration ()
  "Save the current window configuration close other buffers."
  (interactive)
  (meow/save-window-configuration)
  (select-window (split-window))
  (delete-other-windows))

(defun meow/load-window-configuration ()
  "Select a window configuration from the list and load it."
  (interactive)
  (let ((config (cdr
		 (assoc
		  (completing-read "Select window config: " meow/saved-window-configurations)
		  meow/saved-window-configurations))))
    (when config
      (window-state-put config (frame-root-window) t))))

(defun meow/delete-window-configuration ()
  "Select a window configuration to delete."
  (interactive)
  (setq meow/saved-window-configurations
	(delq (assoc
	       (completing-read "Delete a window configuration: "
				meow/saved-window-configurations)
	       meow/saved-window-configurations)
	      meow/saved-window-configurations)))

(provide 'meow/window-configuration)
;;; window-configuration.el ends here
