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

(defmacro meow/runonce (name only-daemon &rest forms)
  "Define a runonce helper NAME, FORMS are executed only once.
Called on `after-init-hook' and `server-after-make-frame-hook'.
If ONLY-DAEMON is set, it's only run on `server-after-make-frame-hook'."
  (let ((flag (intern (format "--meow/runonce-flag-%s" name))))
    `(progn
       ,(unless only-daemon
		  `(add-hook 'after-init-hook (lambda ()
										,@forms)))
       (defvar ,flag nil)
       (add-hook 'server-after-make-frame-hook
				 (lambda ()
				   (unless ,flag
					 ,@forms
					 (setq ,flag t)))))))

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
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

(defun meow/eval-region-and-go-to-normal-mode ()
  "Evaluate elisp in the selected region and go back to normal mode."
  (interactive)
  (if (region-active-p)
      (eval-region (region-beginning) (region-end))
    (eval-region (line-beginning-position) (line-end-position)))
  (evil-normal-state))

(defun meow/turn-off-line-numbers ()
  "Turn off line numbers 🤯."
  (display-line-numbers-mode 0))

(defun add-to-load-path (packages)
  "Add PACKAGES from the Emacs flake to `load-path'."
  (interactive "sPackage(s): ")
  (message "Starting build, please be patient...")
  (let ((path "github:thundertheidiot/emacs")
		(buffer (generate-new-buffer (format "*nix build add-to-load-path %s*" packages)))
		(package-list (split-string packages " ")))
	(make-process
	 :name "nix build add-to-load-path"
	 :buffer buffer
	 :command `("nix" "build"
				"--print-out-paths" "--no-link" "--impure" "--show-trace"
				"--expr"
				,(format (concat
						  "let "
						  "inherit (builtins) filter concatMap isList getAttr;"
						  "isDerivation = v: v.type or null == \"derivation\";"
						  "f = drv: (filter isDerivation drv.propagatedBuildInputs)"
						  " ++ (map f (filter isDerivation drv.propagatedBuildInputs));"
						  "flatten = x: if isList x then concatMap flatten x else [ x ];"
						  "flake = builtins.getFlake \"%s\";"
						  "epkgs = flake.packages.\"${builtins.currentSystem}\".emacs.epkgs;"
						  "get = attrs: name: getAttr name attrs;"
						  "pkgs' = map (get epkgs) [%s];"
						  "pkgs = flatten (pkgs' ++ (map f pkgs'));"
						  "in pkgs")
						 path
						 (mapconcat (lambda (n)
									  (concat "\"" n "\" "))
									package-list)))
	 :sentinel (lambda (proc _event)
				 (when (eq (process-status proc) 'exit)
				   (funcall
					(lambda ()
					  (let* ((paths- (with-current-buffer buffer
									   (string-lines (buffer-string))))
							 (paths (seq-filter
									 (lambda (p)
									   (file-directory-p p))
									 paths-))
							 (lisp (flatten-list
									(mapcar (lambda (p)
											  (directory-files-recursively p
																		   (rx ".el" (? ?c) eol)))
											paths)))
							 (native-lisp (flatten-list
										   (mapcar (lambda (p)
													 (directory-files-recursively p
																				  (rx ".eln" eol)))
												   paths)))
							 (lisp-directories (delete-dups
												(mapcar (lambda (f) (file-name-directory f)) lisp)))
							 (native-lisp-directories (delete-dups
													   (mapcar (lambda (f) (file-name-directory f)) native-lisp))))
						(mapc (lambda (p)
								(add-to-list 'load-path p)
								(message "added %s to load-path" p))
							  lisp-directories)
						(mapc (lambda (p)
								(add-to-list 'native-comp-eln-load-path p))
							  native-lisp-directories)
						(mapc (lambda (package)
								(require (intern package)))
							  package-list)))))))))

(defun meow/async-shell-command-buffer (command callback &optional buffer)
  "Start process for shell COMMAND, call CALLBACK with the process and buffer after exit."
  (let* ((buf (or buffer (generate-new-buffer (format " *meow/async %s*" command))))
		 (proc (start-process (format "async %s" command) buf
							  "bash" "-c" command)))
    (set-process-sentinel
     proc
     (lambda (process _event)
	   (when (eq (process-status process) 'exit)
		 (funcall callback process buf))))
	buf))

(provide 'meow-helpers)
