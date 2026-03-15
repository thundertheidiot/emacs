;; -*- lexical-binding: t; -*-
(with-eval-after-load "ewm"
  (when (file-directory-p (expand-file-name "ewm.el" user-emacs-directory))
	(load-file (expand-file-name "ewm.el" user-emacs-directory)))

  (defvar consult-source-xdg-apps
	`(:name "Apps"
			:narrow ?a
			:category app
			:items ,(lambda ()
					  (mapcar #'car (ewm-list-xdg-apps)))
			:action ,#'ewm-launch-xdg-command))
  (add-to-list 'consult-buffer-sources consult-source-xdg-apps)

  (general-def :keymaps 'ewm-mode-map
	"s-d" #'consult-buffer))

(provide 'meow-ewm)
