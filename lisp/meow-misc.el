;; -*- lexical-binding: t; -*-
(require 'recentf)

(setq-default tab-width 4
			  c-basic-offset 'tab-width)

(setq use-short-answers t
      native-comp-async-report-warnings-errors 'silent

      ;; gc-cons-threshold (* 8 1024 1024)
      read-process-output-max (* 1024 1024)
      inhibit-compacting-font-caches t

      ring-bell-function 'ignore

      inhibit-startup-screen t
      inhibit-splash-screen t

      split-width-threshold 120
      split-height-threshold 40

	  initial-scratch-message ";; -*- lexical-binding: t; -*-\n\n"

      confirm-kill-processes nil

      completion-ignore-case t

	  switch-to-buffer-obey-display-actions t

	  resize-mini-windows t
	  max-mini-window-height 0.5

      world-clock-list '(("Europe/Helsinki" "Finland")
						 ("Europe/London" "UK")
						 ("America/Chicago" "car")
						 ("Europe/Rome" "emax")
						 ("America/Buenos_Aires" "Diza"))

      backward-delete-char-untabify-method nil)

(use-package eldoc
  :ensure nil
  :config
  (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly))

(defun meow/display-buffer-in-side-window-fixed (buffer alist)
  "Like `display-buffer-in-side-window' but enforces WINDOW-HEIGHT/WIDTH
even when an existing window is reused. Claudeslop."
  (let ((window (display-buffer-in-side-window buffer alist)))
    (when window
      (let ((height (cdr (assq 'window-height alist)))
            (width (cdr (assq 'window-width alist))))
        (when height
          (let ((target (if (floatp height)
							(round (* (frame-height) height))
						  height)))
            (window-resize window (- target (window-height window)) nil t)))
        (when width
          (let ((target (if (floatp width)
							(round (* (frame-width) width))
						  width)))
            (window-resize window (- target (window-width window)) nil t)))))
    window))

;; display buffer alist
(setq display-buffer-alist
	  '(((or (major-mode . help-mode)
			 (major-mode . helpful-mode))
		 meow/display-buffer-in-side-window-fixed
		 (reusable-frames . visible)
		 (side . bottom)
		 (window-height . 0.4)
		 (preserve-size . (nil . t)))
		((or (major-mode . grep-mode)
			 "^\\*Embark Export")
		 display-buffer-reuse-window
		 (inhibit-same-window . t))
		("^\\*eldoc"
		 meow/display-buffer-in-side-window-fixed
		 (side . bottom)
		 (window-height . 4)
		 (preserve-size . (nil . t)))
		("\\*compilation\\*"
		 display-buffer-reuse-window
		 (inhibit-same-window . t))))

(electric-indent-mode)
(electric-pair-mode)
(savehist-mode 1)

;; needed to insert ` with my keyboard
(global-set-key (kbd "s-`") (kbd "`"))

;; save recently opened files
(recentf-mode)
(setq recentf-max-menu-items 10000
      recentf-max-saved-items 10000)
(run-at-time "5 min" 300 'recentf-save-list)

(defun advice!-keyboard-escape-quit-adv (fun)
  "Around advice for `keyboard-escape-quit' FUN.
Preserve window configuration when pressing ESC."
  (let ((buffer-quit-function (or buffer-quit-function #'ignore)))
    (funcall fun)))
(advice-add #'keyboard-escape-quit :around #'advice!-keyboard-escape-quit-adv)

;; better pixel scroll
(use-package ultra-scroll
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0
		jit-lock-defer-time 0.05)  ; defer fontification
  :config
  (ultra-scroll-mode 1))

;; this fixes rendering of eldoc for at least haskell language server
(use-package markdown-mode
  :ensure nil
  :demand t
  :mode "\\.md\\'")

(use-package tramp-sh
  :ensure nil ;; part of emacs
  :config
  ;; https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./

  (remove-hook 'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options)

  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))

  (connection-local-set-profiles
   '(:application tramp :protocol "scp")
   'remote-direct-async-process)

  (connection-local-set-profiles
   '(:application tramp :protocol "rsync")
   'remote-direct-async-process)

  (setq tramp-remote-path
		(append tramp-remote-path
 				'(tramp-own-remote-path))
		remote-file-name-inhibit-locks t
		tramp-use-scp-direct-remote-copying t
		remote-file-name-inhibit-auto-save-visited t
		tramp-copy-size-limit (* 2 1024 1024)
		tramp-verbose 2
		magit-tramp-pipe-stty-settings 'pty
		vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)"
									 vc-ignore-dir-regexp
									 tramp-file-name-regexp)))

(use-package dired
  :ensure nil
  :demand t
  :hook (dired-mode . hl-line-mode)
  :hook (dired-mode . auto-revert-mode)
  :hook (dired-mode . meow/turn-off-line-numbers)
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
			"oe" '("eshell in this window" . (lambda () (interactive) (meow/eshell))))
  :config
  (unless (display-graphic-p)
    (general-def dired-mode-map "DEL" 'dired-up-directory)))

(use-package diredfl
  :config
  (diredfl-global-mode))

;; open media files in mpv
(use-package openwith
  :custom
  (openwith-associations `((,(rx nonl
								 (or ".mkv"
									 ".mp4"
									 ".mov"
									 ".webm")
								 eos)
							. ("mpv" (file)))))
  :config
  (setq consult-preview-excluded-files (mapcar #'car openwith-associations))
  (openwith-mode))

;; view pdfs
(use-package pdf-tools
  :after evil-collection
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :hook (pdf-view-mode . (lambda () (display-line-numbers-mode -1)))
  :hook (pdf-view-mode . auto-revert-mode)
  :config
  (evil-collection-pdf-setup))

(use-package helpful
  :init
  (setq apropos-do-all t)
  :bind
  (([remap describe-function] . helpful-callable)
   ([remap describe-command]  . helpful-command)
   ([remap describe-variable] . helpful-variable)
   ([remap describe-key]      . helpful-key)
   ([remap describe-symbol]   . helpful-symbol)
   :map embark-become-help-map
   ([remap describe-function] . helpful-callable)
   ([remap describe-command]  . helpful-command)
   ([remap describe-variable] . helpful-variable)
   ([remap describe-symbol]   . helpful-symbol)
   :map embark-symbol-map
   ("h" . helpful-symbol))
  :general-config
  (:keymaps 'helpful-mode-map :states '(normal visual motion)
			"q" 'evil-quit))

;; editor as new buffer
(setenv "EDITOR" "emacsclient")

(use-package async
  :config
  (require 'dired-async)
  (dired-async-mode 1))

(use-package elfeed
  :general-config
  (meow/leader
	"of" '("elfeed" . elfeed))
  (:keymaps 'elfeed-show-mode-map :states '(normal visual)
			"o" (lambda () (interactive)
				  (unless (eww-suggested-uris)
					(re-search-forward browse-url-button-regexp))
				  (call-interactively #'eww-open-in-new-buffer)))
  (:keymaps 'eww-mode-map :states '(normal visual)
			"q" #'quit-window))

(use-package elfeed-protocol
  :custom
  (elfeed-use-curl t)
  (elfeed-feeds `(("fever+https://thunder@rss.meowcloud.net"
				   :api-url "https://rss.meowcloud.net/api/fever.php"
				   :password-file ,(expand-file-name "elfeed-password" user-emacs-directory))))
  :config
  (elfeed-protocol-enable))

(defun meow/view-url ()
  "View url in the clipboard.
Guess the mode using `auto-mode-alist' with the url."
  (interactive)
  (when (seq-contains-p (gui-get-selection 'CLIPBOARD 'TARGETS) 'text/plain)
	(let* ((clipboard (gui-get-selection 'CLIPBOARD 'text/plain))
		   (buf (get-buffer-create (generate-new-buffer (format "*meow/url %s" clipboard)))))
	  (plz 'get clipboard
		:then (lambda (res)
				(with-current-buffer buf
				  (insert res)
				  (let* ((case-fold-search nil)
						 (mode (assoc-default clipboard auto-mode-alist 'string-match)))
					(when mode
					  (set-auto-mode-0 mode nil))))
				(switch-to-buffer buf)
				(goto-char (point-min)))))))

(provide 'meow-misc)
;;; meow-misc.el ends here
