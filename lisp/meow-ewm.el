;; -*- lexical-binding: t; -*-
(with-eval-after-load "ewm"
  (when (file-directory-p (expand-file-name "ewm.el" user-emacs-directory))
	(load (expand-file-name "ewm.el" user-emacs-directory)))

  (setq ewm-input-config
		'((touchpad :natural-scroll t :tap t :dwt t)
		  (mouse :accel-profile "flat")
		  (keyboard :repeat-delay 300 :repeat-rate 50
					:xkb-layouts "us,fi"
					:xkb-options "grp:win_space_toggle")))

  (setq ewm-mouse-follows-focus t)

  (setq ewm-intercept-prefixes (mapcar (lambda (key)
										 (aref (kbd key) 0))
									   '("M-x"
										 "C-SPC")))

  (ewm--send-input-config)

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
