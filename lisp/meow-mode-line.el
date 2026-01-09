;; -*- lexical-binding: t; -*-

(use-package nyan-mode
  :demand t
  :custom
  (nyan-animate-nyancat t)
  (nyan-wavy-trail t))

(defun meow/mode-line ()
  "Set up custom mode line, this is called on `enable-theme-functions'."
  ;; disable solair mode recoloring for the mode line
  (dolist (face '(mode-line mode-line-active mode-line-inactive))
    (setf (alist-get face solaire-mode-remap-alist) nil))

  (set-face-attribute 'mode-line-active nil :inherit 'mode-line-inactive :foreground (face-attribute 'default :foreground))

  ;; box to make mode line look nicer
  (mapc (lambda (face)
	  (set-face-attribute face nil
			      :box `(:line-width (1 . 10) :color ,(face-background face nil t) :style nil)
			      :height 110))
	'(mode-line-active mode-line-inactive))

  (setq mode-line-format nil)
  (kill-local-variable 'mode-line-format)
  (force-mode-line-update)

  (let* ((default-face `(:foreground ,(face-attribute 'default :foreground)))
	 (okay-face `(:foreground ,(face-attribute 'match :foreground)))
	 (error-face `(:foreground ,(face-attribute 'error :foreground)))
	 (warning-face `(:foreground ,(face-attribute 'warning :foreground)))
	 (emphasize-face `(:foreground ,(face-attribute 'font-lock-keyword-face :foreground)))

	 (envrc-none (propertize "" 'face default-face))
	 (envrc-on (propertize "" 'face okay-face))
	 (envrc-error (propertize "" 'face error-face)))
    (setq-default mode-line-format
		  `("   "
		    (:eval
		     (propertize "%b" 'face 'bold))
		    "   L%l   " ;; line number

		    (:eval
		     (when (file-remote-p default-directory)
		       (let* ((vec (tramp-dissect-file-name default-directory))
			      (user (or (tramp-file-name-user vec) ""))
			      (host (tramp-file-name-host vec)))
			 (propertize (format "%s@%s   " user host) 'face ',emphasize-face))))

		    (:eval
		     (pcase envrc--status
		       ('none ,envrc-none)
		       ('on ,envrc-on)
		       (_ ,envrc-error)))

		    "   "
		    
		    (:eval
		     (when (and (not (file-remote-p default-directory)) (magit-toplevel))
		       (propertize (format "  %s   " (magit-get-current-branch)) 'face ',emphasize-face)))
		    
		    (eglot--managed-mode eglot--mode-line-format "")
		    (eglot--managed-mode "   " "")

		    (flycheck-mode
		     (:eval
		      (when (and (eq flycheck-last-status-change 'finished))
			(let-alist (flycheck-count-errors flycheck-current-errors)
			  (concat
			   (when (and (not .error)  (not .warning))
			     (propertize "" 'face ',okay-face))
			   (when .error
			     (propertize (format " %s" .error) 'face ',error-face))
			   (when .warning
			     (propertize (format "%s %s" (if .error " " "") .warning) 'face ',warning-face))))))
		     
		     "")

		    (flycheck-mode "   " "")

		    ;; "%="
		    (:eval (nyan-create))))))

(add-hook 'enable-theme-functions
	  (lambda (_theme) (meow/mode-line)))

(provide 'meow-mode-line)
;;; meow-mode-line.el ends here
