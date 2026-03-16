;; -*- lexical-binding: t; -*-
(with-eval-after-load "ewm"
  (let ((local-ewm-file (expand-file-name "ewm-local.el" user-emacs-directory)))
    (when (or (file-directory-p local-ewm-file)
    		  (file-symlink-p local-ewm-file))
      (load-file local-ewm-file)))

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

  ;; (ewm--send-input-config)

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
