;; -*- lexical-binding: t; -*-
(defcustom meow/create-directories '()
  "List of directories to create on init."
  :type '(repeat string)
  :group 'meow-emacs)

(defun meow/setup-directories ()
  (mapc (lambda (dir)
	  (unless (file-directory-p dir)
	    (make-directory dir)))
	meow/create-directories))

(add-hook 'after-init-hook #'meow/setup-directories)

(setq split-width-threshold 180)
(setq split-height-threshold 80)

(defun meow/intelligent-split (&optional force)
  (interactive)
  (let* ((width (window-total-width))
	 (height (window-total-height))
	 (aspect (/ (float width) (float height)))
	 (window (cond ((and (< width split-width-threshold) (< height split-height-threshold) (not force)) (current-buffer))
		       ((> aspect 2.3) (split-window-right))
		       (t (split-window-below)))))
    (ignore-errors (balance-windows (window-parent)))
    window))

(provide 'meow/helpers)
