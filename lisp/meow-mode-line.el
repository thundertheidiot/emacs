;; -*- lexical-binding: t; -*-

(use-package nyan-mode
  :demand t
  :custom
  (nyan-animate-nyancat t)
  (nyan-wavy-trail t))

(defface meow/mode-line-okay-face '((t (:inherit mode-line-active)))
  "Modeline okay face.")
(defface meow/mode-line-error-face '((t (:inherit mode-line-active)))
  "Modeline error face.")
(defface meow/mode-line-warning-face '((t (:inherit mode-line-active)))
  "Modeline warning face.")
(defface meow/mode-line-emphasize-face '((t (:inherit mode-line-active)))
  "Modeline emphasize face.")

(defun meow/mode-line-update-faces ()
  "Update mode line faces (run on theme change)."
  (face-spec-set 'meow/mode-line-okay-face `((t
					      :foreground ,(face-attribute 'magit-process-ok :foreground)
					      :inherit mode-line-active)))
  (face-spec-set 'meow/mode-line-error-face `((t
					       :foreground ,(face-attribute 'error :foreground)
					       :inherit mode-line-active)))
  (face-spec-set 'meow/mode-line-warning-face `((t
						 :foreground ,(face-attribute 'warning :foreground)
						 :inherit mode-line-active)))
  (face-spec-set 'meow/mode-line-emphasize-face `((t
						   :foreground ,(face-attribute 'font-lock-keyword-face :foreground)
						   :inherit mode-line-active))))

(defvar-local meow/cached-git-info nil)
(defun meow/invalidate-git-cache (&rest _)
  "Invalidate git cache."
  (setq meow/cached-git-info nil))

(defvar-local meow/mode-line-flycheck nil)
(defun meow/mode-line-flycheck-update (&optional status)
  "Update flycheck text with STATUS."
  (when-let* ((text
	       (pcase status
		 ('finished
		  (if flycheck-current-errors
		      (let* ((errors (flycheck-count-errors flycheck-current-errors))
			     (c-error (alist-get 'error errors))
			     (c-warning (alist-get 'warning errors)))
			(concat
			 (when c-error
			   (propertize (format " %s   " c-error) 'face 'meow/mode-line-error-face))
			 (when c-warning
			   (propertize (format " %s   " c-warning) 'face 'meow/mode-line-warning-face))))
		    (propertize "   " 'face 'meow/mode-line-okay-face)))
		 (_ nil))))
    (setq meow/mode-line-flycheck text)))

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

  ;; update faces
  (meow/mode-line-update-faces)

  (setq mode-line-format nil)
  (kill-local-variable 'mode-line-format)
  (force-mode-line-update)

  (let ((envrc-none (propertize "" 'face 'mode-line-active))
	(envrc-on (propertize "" 'face 'meow/mode-line-okay-face))
	(envrc-error (propertize "" 'face 'meow/mode-line-error-face)))
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
			 (propertize (format "%s@%s   " user host) 'face 'meow/mode-line-emphasize-face))))

		    (:eval
		     (pcase envrc--status
		       ('none ,envrc-none)
		       ('on ,envrc-on)
		       (_ ,envrc-error)))

		    "   "

		    ;; (:eval
		    ;;  (unless (file-remote-p default-directory)
		    ;;    (propertize (format "  %s   " (or meow/cached-git-info
		    ;; 					  (when-let* ((branch (magit-get-current-branch)))
		    ;; 					    (setq meow/cached-git-info branch))))
		    ;; 		   'face ',emphasize-face)))

		    (eglot--managed-mode eglot--mode-line-format "")
		    (eglot--managed-mode "   " "")

		    (meow/mode-line-flycheck (:eval meow/mode-line-flycheck))

		    "%="
		    (:eval (nyan-create))))))

(add-hook 'enable-theme-functions
	  (lambda (_theme) (meow/mode-line)))

;; (advice-add 'magit-checkout :after #'meow/invalidate-git-cache)
;; (add-hook 'find-file-hook #'meow/invalidate-git-cache)

(add-hook 'flycheck-status-changed-functions #'meow/mode-line-flycheck-update)
(add-hook 'flycheck-mode-hook #'meow/mode-line-flycheck-update)

;; (run-with-idle-timer 2 t
;; 		     (lambda ()
;; 		       (when (and buffer-file-name (not (file-remote-p default-directory)))
;; 			 (meow/invalidate-git-cache))))

(provide 'meow-mode-line)
;;; meow-mode-line.el ends here
