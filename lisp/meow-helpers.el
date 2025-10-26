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

(defun meow/comment-or-uncomment-region-or-line ()
  "If a region is selected, either uncomment or comment it, if not, uncomment or comment the current line."
  (interactive)
  (let (beg end)
    (if (region-active-p)
	(setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(defun meow/eval-region-and-go-to-normal-mode ()
  "Evaluate elisp in the selected region and go back to normal mode."
  (interactive)
  (let (beg end)
    (if (region-active-p)
	(setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (eval-region beg end)
    (evil-normal-state)))

(provide 'meow-helpers)
