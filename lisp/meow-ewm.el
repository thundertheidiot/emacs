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

  ;; tab bar as bar
  (setq tab-bar-show t)
  
  (setq tab-bar-format
		'(tab-bar-format-history
		  tab-bar-format-tabs
		  tab-bar-separator
		  
		  tab-bar-format-align-right
		  tab-bar-format-global))

  (require 'time)
  (setq display-time-format " %H:%M ")
  (setq display-time-interval 30)
  (setq display-time-load-average nil)

  (display-time-mode 1)

  (require 'battery)
  (display-battery-mode)

  (defun meow/battery-display ()
	(if-let* ((status (ignore-errors (funcall battery-status-function))))
		(let* ((percentage (string-to-number (alist-get ?\p status)))
			   (charging (string-match-p "Charging" (alist-get ?\B status)))
			   (icon (cond
					  (charging "")
					  ((>= percentage 90) "")
					  ((>= percentage 75) "")
					  ((>= percentage 50) "")
					  ((>= percentage 25) "")
					  (t ""))))
		  (format "%s %s (%s:%s)  "
				  icon
				  (alist-get ?\p status)

				  (alist-get ?\h status)
				  (alist-get ?\m status)))
	  ""))

  (setq global-mode-string '(""
							 display-time-string
							 (:eval (meow/battery-display))))

  (defvar consult-source-xdg-apps
	`(:name "Apps"
			:narrow ?a
			:category app
			:items ,(lambda ()
					  (mapcar #'car (ewm-list-xdg-apps)))
			:action ,#'ewm-launch-xdg-command))
  (add-to-list 'consult-buffer-sources consult-source-xdg-apps)

  (defun meow/ewm-screenshot ()
	(interactive)
	(let ((freeze (start-process "wayfreeze" nil "wayfreeze")))
	  (meow/async-shell-command-buffer
	   "grim -g \"$(slurp)\" - | wl-copy"
	   (lambda (&rest _)
		 (delete-process freeze)
		 (message "Screenshot copied to clipboard"))
	   nil)))

  (general-def :keymaps 'ewm-mode-map
	"s-d" #'consult-buffer
	"<print>" #'meow/ewm-screenshot
	"s-h" #'windmove-left
	"s-j" #'windmove-down
	"s-k" #'windmove-up
	"s-l" #'windmove-right))

(provide 'meow-ewm)
