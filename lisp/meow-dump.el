					; -*- lexical-binding: t; -*-

;; (add-to-list 'default-frame-alist '(alpha-background . 0.88))


(add-function :after after-focus-change-function
	      (defun th/garbage-collect ()
		(unless (frame-focus-state)
		  (garbage-collect))))


;; (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; (global-set-key (kbd "ESC") 'keyboard-escape-quit)


(use-package subr-x :ensure nil)



;; (use-package evil-surround
;;   :config
;;   (global-evil-surround-mode 1))


;; (general-create-definer th/leader
;;   :states '(normal insert visual emacs motion)
;;   :keymaps 'override
;;   :prefix "SPC"
;;   :global-prefix "C-SPC")

;; (general-create-definer th/local
;;   :states '(normal insert visual emacs motion)
;;   :keymaps 'override
;;   :prefix "SPC l"
;;   :global-prefix "C-SPC l")


(use-package separedit)


(defvar th/first-server-frame-created nil)
(defun th--unless-first-server-frame-created (func)
  (unless th/first-server-frame-created
    (funcall func)
    (setq th/first-server-frame-created t)))



;; (add-hook 'window-setup-hook #'th/mode-line)
;; (add-hook 'server-after-make-frame-hook #'th/mode-line)


;; TODO implement conditional system
;; (use-package dmenu
;;   :demand t)

;; ;; custom package by me, defined in default.nix
;; ;; allows for per monitor workspaces to be bound to mod+1-9, like on dwm
;; (use-package dwm-workspaces
;;   :demand t)

;; (use-package exwm
;;   :demand t)

;; (use-package exwm-firefox-evil
;;   :config
;;   (add-hook 'exwm-manage-finish-hook #'exwm-firefox-evil-activate-if-firefox))

;; (when (and (getenv "EMACS_ENABLE_EXWM") (executable-find "wmctrl"))
;;   (unless (eq (call-process "wmctrl" nil nil nil "-m") 0)
;;     (progn
;;       (require 'exwm-randr)
;;       (require 'exwm-systemtray)
;;       (require 'exwm-xim)

;;       (dwm-workspaces--init)

;;       (defun th/exwm-shell-cmd (command) (start-process-shell-command (car (split-string command " ")) nil command))

;;       (defun th/keyboard-layout ()
;; 	(interactive)
;; 	(let* ((output (shell-command-to-string "setxkbmap -query"))
;; 	       (layout (nth 2 (split-string output "\n"))))
;; 	  (if (string-match-p "us" layout)
;; 	      (shell-command-to-string "setxkbmap fi")
;; 	    (shell-command-to-string "setxkbmap us"))))

;;       (mapc 'th/exwm-shell-cmd
;; 	    '("xset r rate 300 50"
;; 	      "dbus-update-activation-environment --verbose --systemd DBUS_SESSION_BUS_ADDRESS DISPLAY XAUTHORITY &"
;; 	      "gnome-keyring-daemon"))

;;       (when (file-directory-p "/sys/class/power_supply/BAT0/")
;; 	(display-battery-mode))

;;       (setq display-time-format "%H:%M:%S - %d %b %Y (%a)"
;; 	    display-time-default-load-average nil

;; 	    mouse-autoselect-window t
;; 	    focus-follow-mouse t

;; 	    exwm-input-line-mode-passthrough t
;; 	    exwm-workspace-show-all-buffers t)
;;       (display-time-mode 1)

;;       (dolist (k `(
;;                    escape
;;                    ))
;;         (cl-pushnew k exwm-input-prefix-keys))


;;       (defun advice!-exwm-input--on-ButtonPress-line-mode (buffer button-event)
;; 	"Handle button events in line mode.
;; BUFFER is the `exwm-mode' buffer the event was generated
;; on. BUTTON-EVENT is the X event converted into an Emacs event.

;; The return value is used as event_mode to release the original
;; button event."
;; 	(with-current-buffer buffer
;; 	  (let ((read-event (exwm-input--mimic-read-event button-event)))
;; 	    (exwm--log "%s" read-event)
;; 	    (if (and read-event
;; 		     (exwm-input--event-passthrough-p read-event))
;; 		;; The event should be forwarded to emacs
;; 		(progn
;; 		  (exwm-input--cache-event read-event)
;; 		  (exwm-input--unread-event button-event)

;; 		  xcb:Allow:ReplayPointer)
;; 	      ;; The event should be replayed
;; 	      xcb:Allow:ReplayPointer))))

;;       (advice-add 'exwm-input--on-ButtonPress-line-mode :override #'advice!-exwm-input--on-ButtonPress-line-mode)

;;       (setq exwm-input-global-keys
;; 	    `((,(kbd "s-i") . exwm-input-toggle-keyboard)
;; 	      (,(kbd "s-f") . exwm-layout-toggle-fullscreen)
;; 	      (,(kbd "s-S-F") . exwm-floating-toggle-floating)
;; 	      (,(kbd "s-d") . dmenu)
;; 	      (,(kbd "s-SPC") . th/keyboard-layout)
;; 	      (,(kbd "<XF86AudioPlay>") . simple-mpc-toggle)

;; 	      ;; (,(kbd "M-x") . execute-extended-command)

;; 	      (,(kbd "s-.") . dwm-workspaces--select-previous-monitor)
;; 	      (,(kbd "s-,") . dwm-workspaces--select-next-monitor)

;; 	      (,(kbd "s-1") . (lambda () (interactive) (dwm-workspaces--switch-by-index 1)))
;; 	      (,(kbd "s-2") . (lambda () (interactive) (dwm-workspaces--switch-by-index 2)))
;; 	      (,(kbd "s-3") . (lambda () (interactive) (dwm-workspaces--switch-by-index 3)))
;; 	      (,(kbd "s-4") . (lambda () (interactive) (dwm-workspaces--switch-by-index 4)))
;; 	      (,(kbd "s-5") . (lambda () (interactive) (dwm-workspaces--switch-by-index 5)))
;; 	      (,(kbd "s-6") . (lambda () (interactive) (dwm-workspaces--switch-by-index 6)))
;; 	      (,(kbd "s-7") . (lambda () (interactive) (dwm-workspaces--switch-by-index 7)))
;; 	      (,(kbd "s-8") . (lambda () (interactive) (dwm-workspaces--switch-by-index 8)))
;; 	      (,(kbd "s-9") . (lambda () (interactive) (dwm-workspaces--switch-by-index 9)))

;; 	      ;; (,(kbd "s-!") . (lambda () (interactive) (dwm-workspaces--move-window-by-index 1)))
;; 	      ;; (,(kbd "s-@") . (lambda () (interactive) (dwm-workspaces--move-window-by-index 2)))
;; 	      ;; (,(kbd "s-#") . (lambda () (interactive) (dwm-workspaces--move-window-by-index 3)))
;; 	      ;; (,(kbd "s-$") . (lambda () (interactive) (dwm-workspaces--move-window-by-index 4)))
;; 	      ;; (,(kbd "s-%") . (lambda () (interactive) (dwm-workspaces--move-window-by-index 5)))
;; 	      ;; (,(kbd "s-^") . (lambda () (interactive) (dwm-workspaces--move-window-by-index 6)))
;; 	      ;; (,(kbd "s-&") . (lambda () (interactive) (dwm-workspaces--move-window-by-index 7)))
;; 	      ;; (,(kbd "s-*") . (lambda () (interactive) (dwm-workspaces--move-window-by-index 8)))
;; 	      ;; (,(kbd "s-(") . (lambda () (interactive) (dwm-workspaces--move-window-by-index 9)))

;;               ;; ,@(mapc (lambda (i)
;;               ;;           (,(kbd (format "s-%d" i)) .
;;               ;;            (lambda () (interactive)
;;               ;;              (dwm-workspaces--switch-by-index ,i))))
;;               ;;         (number-sequence 1 9))

;;               ,@(cl-mapcar (lambda (c n)
;;                              (,(kbd (format "s-%c" c)) .
;;                               (lambda () (interactive)
;;                                 (dwm-workspaces--move-window-by-index ,n))))
;;                            '(?! ?@ ?# ?$ ?% ?^ ?& ?* ?\()
;;                            (number-sequence 1 9))
;; 	      ))

;;       (mapc (lambda (keybind)
;; 	      (global-set-key (car keybind) (cdr keybind)))
;; 	    exwm-input-global-keys)

;;       ;; https://github.com/minad/corfu/discussions/408
;;       (defun advice!corfu-make-frame-with-monitor-awareness (func frame x y width height)
;; 	"Advice `corfu--make-frame` to be monitor-aware, adjusting X and Y according to the focused monitor."
;; 	(let* ((workarea (nth exwm-workspace-current-index exwm-workspace--workareas))
;; 	       (mon-x (oref workarea x))
;; 	       (mon-y (oref workarea y)))
;; 	  (funcall func frame (+ mon-x x) (+ mon-y y) width height)))

;;       (advice-add 'corfu--make-frame :around #'advice!corfu-make-frame-with-monitor-awareness)

;;       (general-def :states '(normal visual motion) :keymaps 'override :prefix "SPC"
;; 	"y" '(:ignore t :wk "exwm")
;; 	"yd" '("dmenu" . dmenu)
;; 	"yf" '("toggle floating" . exwm-floating-toggle-floating))

;;       (general-define-key 
;;        :states '(normal visual visual replace motion emacs operator-pending)
;;        :keymaps 'exwm-mode-map
;;        "<mouse-1>" (lambda () (interactive) (exwm-input--fake-key 'down))
;;        "<mouse-2>" nil
;;        "<mouse-3>" nil
;;        "<down-mouse-1>" nil
;;        "<down-mouse-2>" nil
;;        "<down-mouse-3>" nil

;;        ;; "i" 'exwm-input-release-keyboard

;;        "h" (lambda () (interactive) (exwm-input--fake-key 'left))
;;        "j" (lambda () (interactive) (exwm-input--fake-key 'down))
;;        "k" (lambda () (interactive) (exwm-input--fake-key 'up))
;;        "l" (lambda () (interactive) (exwm-input--fake-key 'right)))

;;       (setq exwm-workspace-warp-cursor t
;; 	    exwm-layout-show-all-buffers t
;; 	    mouse-autoselect-window t
;; 	    focus-follows-mouse t)

;;       (setq ibuffer-saved-filter-groups
;; 	    '(("default"
;; 	       ("Process" (mode . comint-mode))
;; 	       )))

;;       (add-hook 'ibuffer-mode-hook
;; 		(lambda ()
;; 		  (ibuffer-switch-to-saved-filter-groups "default")))

;;       (add-hook 'exwm-update-class-hook
;; 		(lambda ()
;; 		  (if exwm-class-name
;; 		      (exwm-workspace-rename-buffer exwm-class-name)
;; 		    (exwm-workspace-rename-buffer (generate-new-buffer-name "EXWM - Unknown window")))
;; 		  (exwm-workspace-rename-buffer (format "EXWM - %s" exwm-class-name))))

;;       (exwm-xim-mode 1)
;;       (exwm-randr-mode 1)
;;       (exwm-enable)
;;       (exwm-systemtray-mode 1))))
(provide 'meow-dump)
