;; -*- lexical-binding: t; -*-
(use-package flycheck
  :demand t
  :hook
  (prog-mode . flycheck-mode)
  (eglot-managed-mode . flycheck-mode)
  (flycheck-mode . (lambda () (flycheck-set-indication-mode 'left-fringe)))
  :config
  (add-to-list 'display-buffer-alist
			   '((major-mode . flycheck-error-list-mode)
				 meow/display-buffer-in-side-window-fixed
				 (reusable-frames . visible)
				 (side . bottom)
				 (window-height . 0.15)
				 (preserve-size . (nil . t))))
  :general-config
  (meow/leader
    "cn" '("next error" . flycheck-next-error)
    "cN" '("previous error" . flycheck-previous-error)
	"ce" '("show errors" . flycheck-list-errors)))

;; consult menu for flycheck errors
(use-package consult-flycheck
  :general
  (meow/leader
	"sd" '("flycheck" . consult-flycheck)))

;; integrate flycheck with eglot
(use-package flycheck-eglot
  :demand t
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))

;; autocompletion
(use-package corfu
  :demand t
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.25)
  (corfu-cycle t)
  :hook
  (after-init . global-corfu-mode)
  :config
  (require 'corfu-autoloads)
  :general-config
  (:states '(insert)
		   "C-j" nil
		   "C-k" nil)
  (:states '(normal visual insert) :keymaps 'corfu-mode-map
		   "C-j" nil
		   "C-k" nil
		   "C-i" nil)
  (:keymaps 'corfu-map
			"RET" nil
			"<up>" nil
			"<down>" nil
			"<tab>" nil
			"TAB" nil
			"M-i" (lambda () (interactive)
					(let ((current-prefix-arg t))
					  (call-interactively #'corfu-info-documentation)))
			"C-j" #'corfu-next
			"C-k" #'corfu-previous
			"S-RET" #'corfu-complete
			"S-<return>" #'corfu-complete
			"C-RET" #'corfu-complete
			"C-<return>" #'corfu-complete))

(use-package nerd-icons-corfu
  :config
  (setf (alist-get 'function nerd-icons-corfu-mapping)
		'(:style "md" :icon "function" :face font-lock-function-name-face))

  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(defun meow/complete-with-consult ()
  "Start `completion-at-point' with `consult-completion-in-region'."
  (interactive)
  (let ((completion-in-region-function #'consult-completion-in-region)
		(completion-ignore-case t))
	(if (eglot-managed-p)
		(cape-interactive #'eglot-completion-at-point)
	  (completion-at-point))))

(general-def :states '(normal visual insert)
  "M-i" #'eldoc
  "M-c" #'meow/complete-with-consult)

(use-package cape
  :demand t
  :config
  (require 'cape-keyword)
  (setq cape-dabbrev-buffer-function
		(lambda () (list (current-buffer)))))

;; automatic formatting
(use-package apheleia
  :demand t
  :config
  (setf (alist-get 'nixfmt apheleia-formatters)
		'("alejandra"))
  (apheleia-global-mode +1))

;; project management
(use-package project
  :ensure nil ;; built in
  :custom
  (project-switch-commands 'project-dired)
  :general-config
  (meow/leader
	"p" '(:ignore t :wk "project")
	"pp" '("switch project" . project-switch-project)
	"ps" '("search" . (lambda () (interactive)
						(consult-ripgrep (project-root (project-current)))))
	"p." '("find file" . project-find-file)
	"pb" '("switch to buffer" . consult-project-buffer)
	"po" '(:ignore t :wk "open")))

(use-package ibuffer-project
  :hook
  (ibuffer . (lambda ()
			   (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
			   (unless (eq ibuffer-sorting-mode 'project-file-relative)
				 (ibuffer-do-sort-by-project-file-relative)))))

(use-package hl-todo
  :demand t
  :config
  (add-hook 'ultra-scroll-hide-functions #'hl-todo-mode)
  (global-hl-todo-mode 1))

(add-hook 'prog-mode-hook (lambda () (hl-line-mode 1)))

(use-package envrc
  :demand t
  :hook (after-init . envrc-global-mode)
  :config
  (require 'info))

(use-package lispyville
  :hook (emacs-lisp-mode . lispyville-mode)
  :hook (common-lisp-mode . lispyville-mode)
  :config
  (lispyville-set-key-theme
   '((operators normal)
	 prettify
	 commentary
	 (slurp/barf-lispy normal))))

(defvar meow/tmc-dir (expand-file-name "~/.local/share/tmc/tmc_cli_rust"))

(defun meow/tmc (args)
  "Call tmc as a shell command with ARGS."
  (let ((default-directory (or
							(when (file-in-directory-p default-directory meow/tmc-dir)
							  (locate-dominating-file default-directory ".tmcproject.yml")))))
    (async-shell-command (format "tmc %s" args))))

(defun meow/--tmc-pick-exercise (course callback)
  "Pick exercise from COURSE, call CALLBACK with the string."
  (meow/async-shell-command-buffer
   (format "tmc exercises %s" course)
   (lambda (proc buf)
     (let* ((string (with-current-buffer buf
					  (goto-char (point-min))
					  (search-forward "Soft deadline:")
					  (search-forward "\n")
					  (buffer-substring (point) (point-max))))
			(exercise (completing-read "Pick exercise: "
									   (seq-filter (lambda (s) (not (string-empty-p s)))
												   (mapcar #'s-trim
														   (split-string string "\n"))))))
       (when exercise
		 (funcall callback (cadr (split-string exercise ": "))))))))

(defun meow/--tmc-open-exercise (course)
  "Open exercise from COURSE with `find-file'."
  (meow/--tmc-pick-exercise
   course
   (lambda (exercise)
     (find-file
      (expand-file-name exercise (expand-file-name course meow/tmc-dir))))))

(defun meow/tmc-pick-exercise (&optional arg)
  "Open a tmc exercise, if you are in an exercise directory pick from the current course, unless ARG is set."
  (interactive "P")
  (if (and (not arg) (file-in-directory-p default-directory meow/tmc-dir))
      (let ((course (car (split-string
						  (file-relative-name default-directory meow/tmc-dir)
						  "/" t))))
		(meow/--tmc-open-exercise course))
    (meow/async-shell-command-buffer
     "tmc courses"
     (lambda (_proc buf)
       (let* ((string (with-current-buffer buf
						(buffer-string)))
			  (course (completing-read "Pick course: "
									   (seq-filter
										(lambda (s) (and (not (string-empty-p s))
														 (not (s-contains-p "Updates" s))))
										(split-string string "\n")))))
		 (meow/--tmc-open-exercise course))))))

(provide 'meow-programming)
;;; meow-programming.el ends here
