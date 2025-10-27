(setq use-short-answers t
      native-comp-async-report-warnings-errors 'silent
      indent-tabs-mode t
      c-basic-offset 'tab-width
      tab-width 4
      ;; gc-cons-threshold (* 8 1024 1024)
      read-process-output-max (* 1024 1024)

      ring-bell-function 'ignore

      inhibit-startup-screen t
      inhibit-splash-screen t

      split-width-threshold 180
      split-height-threshold 80

      backward-delete-char-untabify-method nil)

(electric-indent-mode)
(electric-pair-mode)
(savehist-mode 1)

;; needed to insert ` with my keyboard
(global-set-key (kbd "s-`") #'(lambda () (interactive) (insert "`")))

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

;; scrolling
;; (setq pixel-scroll-precision-large-scroll-height 40.0)
;; (setq pixel-scroll-precision-use-momentum t)

(use-package ultra-scroll
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0) 
  :config
  (ultra-scroll-mode 1))

(defun add-to-load-path (package)
  "Add `package' from your flake to load-path."
  (let ((path "github:thundertheidiot/emacs"))
    (with-temp-buffer
      (let ((exit-code (call-process "nix" nil (list t nil) nil
				     "build"
				     "--print-out-paths"
				     "--impure"
				     "--expr"
				     (format
				      (concat
				       "let "
				       "flake = builtins.getFlake \"%s\";"
				       "epkgs = flake.packages.\"${builtins.currentSystem}\".emacs.epkgs;"
				       "in epkgs.\"%s\"")
				      path
				      package))))
	(if (eq exit-code 0)
	    (let* ((store-path (substring (buffer-string) 0 -1))
		   (path (concat store-path "/share/emacs/site-lisp"))
		   (files (directory-files-recursively path "\\.elc?$"))
		   (directories (mapcar
				 (lambda (file) (file-name-directory file))
				 files))
		   (final-list (delete-dups directories)))
	      (mapc (lambda (path)
		      (add-to-list 'load-path path))
		    final-list)
	      (message (format "Added %s to load path" final-list)))
	  (message "Nix process failed"))))))

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
	    "oe" '("eshell in this window" . (lambda () (interactive) (meow/eshell))))
  :config
  (unless (display-graphic-p)
    (general-def dired-mode-map "DEL" 'dired-up-directory)))

(use-package dired-du)

;; open media files in mpv
(use-package openwith
  :custom
  (openwith-associations `((,(rx nonl (or ".mkv"
					  ".mp4"
					  ".mov"
					  ".webm"))
			    . ("mpv" (file)))))
  :config
  (openwith-mode))

;; view pdfs
(use-package pdf-tools
  :after evil-collection
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :hook (pdf-view-mode . (lambda () (display-line-numbers-mode -1)))
  :config
  (evil-collection-pdf-setup))

(provide 'meow-misc)
;;; meow-misc.el ends here
