					; -*- lexical-binding: t; -*-

;; (add-to-list 'default-frame-alist '(alpha-background . 0.88))

(use-package diminish
  :demand t
  :config
  (diminish 'font-lock-mode)
  (diminish 'visual-line-mode)
  (diminish 'auto-revert-mode)
  (diminish 'eldoc-mode))

(use-package pcre2el)
(use-package dash)
(use-package plz)

(add-function :after after-focus-change-function
	      (defun th/garbage-collect ()
		(unless (frame-focus-state)
		  (garbage-collect))))


;; (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; (global-set-key (kbd "ESC") 'keyboard-escape-quit)


(use-package subr-x :ensure nil)


(defun th/turn-off-line-numbers ()
  "Turn off line numbers 🤯"
  (display-line-numbers-mode 0))


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

(use-package org
  :demand t
  :mode ("\\.org\\'" . org-mode)
  :diminish org-indent-mode
  :custom
  (org-src-preserve-indentation t)
  (org-src-tab-acts-natively t)
  (org-startup-with-inline-images t)
  (org-log-done 'time)
  (calendar-week-start-day 1)
  (org-babel-load-languages '((emacs-lisp . t)
			      (shell . t)
			      (eshell . t)
			      (lisp . t)))
  :init
  (add-hook 'org-mode-hook #'org-indent-mode)
  (add-hook 'org-mode-hook (lambda () (electric-indent-local-mode -1)))
  :general
  (:states '(normal visual motion) :keymaps 'override :prefix "SPC"
	   "oa" '("org agenda" . org-agenda))
  :general-config
  (:keymaps 'org-mode-map
	    "C-j" nil)
  (:states '(normal visual motion) :keymaps 'org-mode-map :prefix "SPC l"
	   "l" '(:ignore t :wk "org link")
	   "li" '("insert org link" . org-insert-link)
	   "lo" '("open org link" . org-open-at-point)
	   "le" '("open org link" . org-edit-special)
	   "lt" '("toggle link display" . org-toggle-link-display))
  (:keymaps 'org-mode-map :states '(normal visual motion)
	    "RET" (lambda () (interactive)
		    (unless (ignore-errors (org-open-at-point))
		      (evil-ret)))))

(use-package evil-org
  :after org
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-roam
  :custom
  (org-roam-directory (file-truename "~/Documents/org/roam"))
  :config
  (unless (file-directory-p org-roam-directory)
    (make-directory org-roam-directory t))
  (setq org-agenda-files (org-roam-list-files))
  (org-roam-db-autosync-mode)
  (org-roam-setup)
  :general
  (:states '(normal visual motion) :keymaps 'override :prefix "SPC"
	   "r" '(:ignore t :wk "roam")
	   "rb" '("buffer" . org-roam-buffer-toggle)
	   "rf" '("find node" . org-roam-node-find)
	   "rI" '("create id" . org-id-get-create)
	   "ri" '("insert node" . org-roam-node-insert)))

(use-package org-download
  :hook (dired-mode . org-download-enable)
  :custom (org-download-screenshot-method "grim -g \"$(slurp)\" -t png %s")
  :general
  (:states '(normal visual motion) :keymaps 'org-mode-map :prefix "SPC l"
	   "s" '("screenshot" . org-download-screenshot)
	   "c" '("image from clipboard" . org-download-clipboard)))

(defun th/org-tempo-electric-pair-fix ()
  (setq-local electric-pair-inhibit-predicate
	      `(lambda (c)
		 (if (char-equal c ?<)
		     t
		   (,electric-pair-inhibit-predicate c)))))

(use-package org-tempo
  :demand t
  :ensure nil ;; included with org
  :after org
  :hook (org-mode . th/org-tempo-electric-pair-fix)
  :custom
  (org-structure-template-alist '(("el" . "src emacs-lisp"))))

(use-package org-bullets
  :diminish org-bullets-mode
  :hook (org-mode . org-bullets-mode))

(use-package olivetti
  :diminish olivetti-mode
  :custom 
  (olivetti-min-body-width 50)
  (olivetti-body-width 80)
  (olivetti-style 'fancy)
  (olivetti-margin-width 12)
  :config
  (set-face-attribute 'olivetti-fringe nil :background "#313244")
  :hook
  (olivetti-mode-on . (lambda () (olivetti-set-width olivetti-body-width)))
  (org-mode . olivetti-mode))

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :hook (pdf-view-mode . (lambda () (display-line-numbers-mode -1)))
  :config
  (evil-collection-pdf-setup))


(general-def :states '(normal visual motion) :keymaps 'override :prefix "SPC"
  "m" '(:ignore t :wk "media"))

(use-package simple-mpc
  :demand t
  :hook (simple-mpc-mode . th/turn-off-line-numbers)
  :custom
  (simple-mpc-volume-step-size 3)
  ;; :general
  ;; (:states '(normal visual motion) :keymaps 'override :prefix "SPC" 
  ;;   "m" '(:ignore t :wk "media")
  ;;   "mm" '("open simple-mpc" . simple-mpc)
  ;;   "ms" '("search" . simple-mpc-query)
  ;;   "mp" '("play/pause" . simple-mpc-toggle)
  ;;   "mC" '("clear" . simple-mpc-clear-current-playlist)
  ;;   "mP" '("playlist" . simple-mpc-view-current-playlist)
  ;;   "ma" '("load playlist" . simple-mpc-load-playlist)
  ;;   "mh" '("prev" . simple-mpc-prev)
  ;;   "ml" '("next" . simple-mpc-next)))
  )

(use-package empv
  :demand t
  :general-config
  (:keymaps 'empv-youtube-results-mode :states '(normal visual insert)
	    "RET" 'empv-youtube-results-play-current)
  :init 
  (setq empv-invidious-instance "https://yewtu.be/api/v1")
  (setq empv-volume-step 3))

(defun eshell/yt (&rest args)
  (empv-youtube (mapconcat (lambda (s) (format "%s " s)) args)))

(use-package transient)

;; (defun media-menu--empv-remove-playlist-item ()
;;   (interactive)
;;   (empv--playlist-select-item-and
;;    (empv-playlist-remove item)))

(transient-define-prefix empv-menu ()
  "Transient menu for empv."
  [["Menu"
    ("q" "Quit" transient-quit-one)]
   ["Playback"
    ("p" "Toggle" empv-toggle :transient t)
    ("v" "Toggle Video" empv-toggle-video :transient t)

    ("j" "Previous" empv-playlist-prev :transient t)
    ("k" "Next" empv-playlist-next :transient t)

    ("x" "Close MPV" empv-exit :transient t)]
   ["Playlist"
    ("Y" "Search Youtube" empv-youtube)
    ("f" "Play File" empv-play-file)
    ("s" "Select From Playlist" empv-playlist-select)]
   ["Settings"
    ("y" "Toggle Single" empv-toggle-file-loop :transient t)
    ("r" "Toggle Repeat" empv-toggle-playlist-loop :transient t)
    
    ("-" "Volume Down" empv-volume-down :transient t)
    ("=" "Volume Up" empv-volume-up :transient t)]])

(transient-define-prefix mpd-menu ()
  "Transient menu for empv."
  [["Menu"
    ("q" "Quit" transient-quit-one)]
   ["Playback"
    ("p" "Toggle" simple-mpc-toggle :transient t)
    
    ("j" "Previous" simple-mpc-prev :transient t)
    ("k" "Next" simple-mpc-next :transient t)]
   ["Playlist"
    ("l" "Load playlist" simple-mpc-load-playlist :transient t)
    ("s" "Search" simple-mpc-query)
    
    ("c" "View playlist" simple-mpc-view-current-playlist)
    ("C" "Clear playlist" simple-mpc-clear-current-playlist :transient t)]
   ["Settings"
    ("y" "Toggle Single" (lambda () (interactive)
			   (simple-mpc-call-mpc nil "single")) :transient t)
    ("r" "Toggle Repeat" simple-mpc-toggle-repeat :transient t)

    ("-" "Volume Down" simple-mpc-decrease-volume :transient t)
    ("=" "Volume Up" simple-mpc-increase-volume :transient t)]])

(defmacro media-menu--mpv-or-mpd-action (mpv mpd &optional mpv-args mpd-args)
  "If the mpv playlist is not empty, call MPV, else call MPD.
MPV is called with MPV-ARGS and MPD is called with MPD-ARGS."
  `(empv--send-command
    '("get_property_string" "playlist")
    (lambda (result)
      (if (> (length (json-parse-string result)) 0)
	  (apply ,mpv ,mpv-args)
	(apply ,mpd ,mpd-args)))))

(defun media-menu--toggle ()
  (interactive)
  (media-menu--mpv-or-mpd-action #'empv-toggle #'simple-mpc-toggle))

(defun media-menu--volume-increase()
  (interactive)
  (media-menu--mpv-or-mpd-action #'empv-volume-up #'simple-mpc-increase-volume))

(defun media-menu--volume-decrease()
  (interactive)
  (media-menu--mpv-or-mpd-action #'empv-volume-down #'simple-mpc-decrease-volume))

(transient-define-prefix media-menu ()
  "Transient menu for simple-mpc and empv."
  [["Menu"
    ("q" "Quit" transient-quit-one)
    ("e" "Open MPV menu" empv-menu)
    ("m" "Open MPD menu" mpd-menu)]
   ["Playback"
    ("p" "toggle playback" media-menu--toggle :transient t)
    ("p" "toggle playback" media-menu--toggle :transient t)
    ]
   ["Settings"
    ("-" "Volume down" media-menu--volume-decrease :transient t)
    ("=" "Volume up" media-menu--volume-increase :transient t)]])

(general-def :states '(normal visual motion) :keymaps 'override :prefix "SPC" 
  "m" '("media menu" . media-menu))

(use-package separedit)

(use-package tramp-sh
  :ensure nil ;; part of emacs
  :config
  (setq tramp-remote-path
	(append tramp-remote-path
 		'(tramp-own-remote-path))))

(use-package dired
  :ensure nil
  :demand t
  :hook (dired-mode . hl-line-mode)
  :hook (dired-mode . auto-revert-mode)
  :custom
  (dired-dwim-target t)
  (dired-mouse-drag-files t)
  (dired-listing-switches "-alh")
  :general-config
  (:keymaps 'dired-mode-map :states '(normal insert visual motion)
	    "SPC" nil
	    "q" 'evil-quit
	    "<backspace>" 'dired-up-directory
	    "C-<return>" (lambda () (interactive) (empv-play (dired-get-filename))))
  (:keymaps 'dired-mode-map :states '(normal visual motion) :prefix "SPC"
	    "oe" '("eshell in this window" . (lambda () (interactive) (th/eshell))))
  :config
  (unless (display-graphic-p)
    (general-def dired-mode-map "DEL" 'dired-up-directory)))

(use-package dired-du)

(use-package openwith
  :custom
  (openwith-associations `((,(rx nonl (or ".mkv"
					  ".mp4"
					  ".mov"
					  ".webm"))
			    . ("mpv" (file)))
			   ))
  :config
  (openwith-mode))

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
(provide 'meow/dump)
