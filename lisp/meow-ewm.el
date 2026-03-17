;; -*- lexical-binding: t; -*-

(defun meow/ewm-switch-monitor-by-direction (dir &optional wrap)
  "Switch to monitor by horizontal direction.
If DIR is truthy, switch one to the right, else go left.
WRAP wraps around."
  (let* ((frame-list
		  (sort
		   (mapcar (lambda (f)
					 (frame-monitor-attributes f))
				   (seq-filter
					;; filter out the corfu frame
					(lambda (f)
					  (not (eq f corfu--frame)))
					(frame-list)))
		   :lessp
		   (lambda (a b)
			 (< (car (alist-get 'geometry a)) (car (alist-get 'geometry b))))))
		 (current-frame (frame-monitor-attributes))
		 (pos (cl-position (alist-get 'name current-frame) frame-list
						   :key (lambda (f) (alist-get 'name f))
						   :test #'string=))
		 (len (length frame-list))
		 (selection
		  (cond
		   ((= len 1) nil)
		   ((and (not wrap) (not dir) (= pos 0)) nil)
		   ((and wrap (not dir) (= pos 0)) (nth (1- len) frame-list))
		   ((and (not wrap) dir (= pos (1- len))) nil)
		   ((and wrap dir (= pos (1- len))) (car frame-list))
		   
		   ((not dir) (nth (1- pos) frame-list))
		   (dir (nth (1+ pos) frame-list)))))
	(when (listp selection) ;; frame-monitor-attributes returns a list
	  (let ((target-frame (car (alist-get 'frames selection))))
		(if (frame-live-p target-frame)
			(select-frame-set-input-focus target-frame)
		  (message "frame isn't alive"))))))

(defun meow/ewm-screenshot ()
  "Take a screenshot to the clipboard using wayfreeze, grim, slurp and wl-copy."
  (interactive)
  (let (;; both are started at the same time
		(freeze (start-process "wayfreeze" nil "wayfreeze"))
		(screenshot (start-process-shell-command "screenshot" nil "grim -g \"$(slurp)\" - | wl-copy")))
	(set-process-sentinel
	 freeze
	 ;; kill screenshot on wayfreeze death (pressing escape)
	 (lambda (freeze &rest _)
	   (unless (process-live-p freeze)
		 (delete-process screenshot))))
	(set-process-sentinel
	 screenshot
	 (lambda (process status)
	   (unless (process-live-p process)
		 ;; kill freeze on screenshot completion, if it's still alive
		 (when (process-live-p freeze)
		   (delete-process freeze))
		 (when (string-match-p "finished" status)
		   (message "Screenshot copied to clipboard")))))))

(defmacro meow/ewm-shell-command (command)
  "Generate an interactive macro to execute shell COMMAND."
  `(lambda () (interactive)
	 (start-process-shell-command "ewm shell command" nil ,command)))

(with-eval-after-load "ewm"
  (let ((local-ewm-file (expand-file-name "ewm-local.el" user-emacs-directory)))
    (when (or (file-directory-p local-ewm-file)
    		  (file-symlink-p local-ewm-file))
      (load-file local-ewm-file)))

  (setq focus-follows-mouse t)
  (setq mouse-autoselect-window t)

  (setq ewm-input-config
		'((touchpad :natural-scroll t :tap t :dwt t)
		  (mouse :accel-profile "flat")
		  (keyboard :repeat-delay 300 :repeat-rate 50
					:xkb-layouts "us,fi"
					:xkb-options "grp:win_space_toggle")))

  (setopt ewm-mouse-follows-focus t)
  (setopt ewm-focus-follows-mouse t)

  (setq ewm-intercept-prefixes (mapcar (lambda (key)
										 (aref (kbd key) 0))
									   '("M-x"
										 "C-SPC"
										 "<print>"
										 "<AudioRaiseVolume>"
										 "<AudioLowerVolume>"
										 "s-,"
										 "s-.")))

  ;; tab bar as bar
  (setq tab-bar-show t)
  
  (setq tab-bar-format
		'(tab-bar-format-history
		  tab-bar-format-tabs
		  tab-bar-separator
		  
		  tab-bar-format-align-right
		  tab-bar-format-global))

  (require 'time)
  (display-time-mode 1)

  (setq display-time-format " %H:%M ")
  (setq display-time-interval 30)
  (setq display-time-load-average nil)

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
		  (format "%s %s (%s)  "
				  icon
				  (alist-get ?\p status)

				  (alist-get ?\t status)))
	  ""))

  (setq global-mode-string '(""
							 display-time-string
							 (:eval (meow/battery-display))))

  ;; consult app launcher
  (defvar consult-source-xdg-apps
	`(:name "Apps"
			:narrow ?a
			:category app
			:hidden t
			:items ,(lambda ()
					  (mapcar #'car (ewm-list-xdg-apps)))
			:action ,#'ewm-launch-xdg-command))
  (add-to-list 'consult-buffer-sources consult-source-xdg-apps)

  (general-def :keymaps 'ewm-mode-map
	"s-d" #'consult-buffer
	"<print>" #'meow/ewm-screenshot
	"<AudioRaiseVolume>" (meow/ewm-shell-command "wpctl set-volume @DEFAULT_AUDIO_SINK@ 3%+")
	"<AudioLowerVolume>" (meow/ewm-shell-command "wpctl set-volume @DEFAULT_AUDIO_SINK@ 3%-")
	"s-h" #'windmove-left
	"s-j" #'windmove-down
	"s-k" #'windmove-up
	"s-l" #'windmove-right
	"s-," (lambda () (interactive)
			(meow/ewm-switch-monitor-by-direction nil))
	"s-." (lambda () (interactive)
			(meow/ewm-switch-monitor-by-direction t))))

(provide 'meow-ewm)
