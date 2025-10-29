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

(defun meow/turn-off-line-numbers ()
  "Turn off line numbers 🤯."
  (display-line-numbers-mode 0))

(defun add-to-load-path (package)
  "Add PACKAGE from the Emacs flake to `load-path'."
  (interactive "sPackage: ")
  (message "Starting build, please be patient...")
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


(provide 'meow-helpers)
