;; -*- lexical-binding: t; -*-
(use-package org
  :demand t
  :mode ("\\.org\\'" . org-mode)
  :custom
  (org-src-preserve-indentation t)
  (org-src-tab-acts-natively t)
  (org-startup-with-inline-images t)
  (org-log-done 'time)
  (org-tags-exclude-from-inheritance '("agenda"))
  (calendar-week-start-day 1)
  (org-babel-load-languages '((emacs-lisp . t)
			      (shell . t)
			      (eshell . t)
			      (lisp . t)))

  (org-agenda-span 7)
  (org-agenda-start-day "+0d")
  (org-agenda-category-icon-alist `(("school" ,(list (all-the-icons-faicon "graduation-cap" :height 0.8)) nil nil :ascent center)
				    ("project" ,(list (all-the-icons-faicon "certificate" :height 0.8)) nil nil :ascent center)
				    ("game" ,(list (all-the-icons-faicon "gamepad" :height 1.0)) nil nil :ascent center)
				    ))
  (org-agenda-prefix-format
   `((agenda . " %i %?-12t% s")
     (todo . " %i %-12:c")
     (tags . " %i %-12:c")
     (search . " %i %-12:c")))
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
  :demand t
  :after org
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-roam
  :demand t ;; load immediately because org agenda depends on org roam
  :custom
  (org-roam-directory (file-truename "~/Documents/org/roam"))
  :config
  (unless (file-directory-p org-roam-directory)
    (make-directory org-roam-directory t))
  (org-roam-db-autosync-mode)
  (org-roam-setup)
  :general
  (:states '(normal visual motion) :keymaps 'override :prefix "SPC"
	   "r" '(:ignore t :wk "roam")
	   "rb" '("buffer" . org-roam-buffer-toggle)
	   "rf" '("find node" . org-roam-node-find)
	   "rI" '("create id" . org-id-get-create)
	   "ri" '("insert node" . org-roam-node-insert)))

;; https://www.d12frosted.io/posts/2021-01-16-task-management-with-roam-vol5
(defun meow/org-set-agenda-tag ()
  "Set the agenda tag for the current org document, if TODOs exist."
  (if (org-element-map
	  (org-element-parse-buffer 'headline)
	  'headline
	(lambda (h)
	  (eq (org-element-property :todo-type h)
	      'todo))
	nil 'first-match)
      (org-roam-tag-add '("agenda"))
    (ignore-errors
      (org-roam-tag-remove '("agenda")))))

(defun meow/org-roam-update-all-agenda-tags ()
  (dolist (file (org-roam-list-files))
    (with-current-buffer (find-file-noselect file)
      (meow/org-update-agenda-tag))))

(defun meow/org-update-agenda-tag ()
  "Check if conditions are met and set the agenda tag."
  (when (and (not (active-minibuffer-window))
	     (derived-mode-p 'org-mode)
	     buffer-file-name
	     (string-prefix-p
	      (expand-file-name (file-name-as-directory org-roam-directory))
	      (file-name-directory buffer-file-name)))
    (meow/org-set-agenda-tag)))

(add-hook 'org-mode-hook #'meow/org-update-agenda-tag)
(add-hook 'before-save-hook #'meow/org-update-agenda-tag)

(defun meow/org-roam-get-agenda-files ()
  "Get list of org roam nodes with the `agenda' tag."
  (org-roam-db-query
   [:select [nodes:file]
	    :from tags
	    :left-join nodes
	    :on (= tags:node-id nodes:id)
	    :where (like tag (quote "%\"agenda\"%"))]))

(defun meow/org-update-agenda-files (&rest _)
  "Update agenda files list."
  (setq org-agenda-files (mapcar #'car (meow/org-roam-get-agenda-files))))

(advice-add 'org-agenda :before #'meow/org-update-agenda-files)
(advice-add 'org-todo-list :before #'meow/org-update-agenda-files)

(use-package org-download
  :hook (dired-mode . org-download-enable)
  :custom (org-download-screenshot-method "grim -g \"$(slurp)\" -t png %s")
  :general
  (:states '(normal visual motion) :keymaps 'org-mode-map :prefix "SPC l"
	   "s" '("screenshot" . org-download-screenshot)
	   "c" '("image from clipboard" . org-download-clipboard)))

(use-package alert
  :custom
  (alert-default-style 'notifications))

(use-package org-wild-notifier
  :custom
  (org-wild-notifier-keyword-blacklist '("DONE"))
  :config
  (org-wild-notifier-mode))

(defun meow/org-tempo-electric-pair-fix ()
  (setq-local electric-pair-inhibit-predicate
	      `(lambda (c)
		 (if (char-equal c ?<)
		     t
		   (,electric-pair-inhibit-predicate c)))))

(use-package org-tempo
  :demand t
  :ensure nil ;; included with org
  :after org
  :hook (org-mode . meow/org-tempo-electric-pair-fix)
  :custom
  (org-structure-template-alist '(("el" . "src emacs-lisp"))))

(use-package org-superstar
  :custom
  (org-superstar-special-todo-items t)
  :hook (org-mode . org-superstar-mode))

(use-package olivetti
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

(defun meow/--org-create-todo (buffer &optional arg)
  "Create TODO entry in BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (let ((top-headings '()))

	(org-map-entries
	 (lambda ()
	   (when (not (or (org-entry-is-todo-p)
			  (org-entry-is-done-p)))
	     (push (org-get-heading) top-headings))))

	(let* ((heading (completing-read "Heading: " (cons "-- Top Level --" (nreverse top-headings))))
	       (name (completing-read "Todo: " nil)))
	  (if (string= heading "-- Top Level --")
	      (progn
		(goto-char (point-max))
		(org-insert-heading '(16) t 1))
	    (progn
	      (goto-char (point-min))
	      (re-search-forward (concat "^\\* " (regexp-quote heading) "$"))
	      (org-insert-heading '(16) t (+ (org-current-level) 1))))
	  (insert (format "TODO %s" name))
	  (pcase arg
	    (`(4) (org-schedule nil))
	    (`(16))
	    (_ (org-deadline nil)))))
      (save-buffer))))

(defun meow/org-add-todo (&optional arg force-roam-find)
  "Add a TODO to an org roam document."
  (interactive "P")
  (if (and (not force-roam-find) (derived-mode-p 'org-mode))
      (meow/--org-create-todo (current-buffer) arg)
    (if-let* ((node (org-roam-node-read))
	      (file (org-roam-node-file node)))
	(meow/--org-create-todo (find-file-noselect file) arg)
      (user-error "Create an org roam node first"))))

(meow/leader
  "ot" '("create todo" . meow/org-add-todo)
  "oT" '("create todo for node" . (lambda () (interactive) (meow/org-add-todo current-prefix-arg t))))

(provide 'meow-org)
;;; meow-org.el ends here
