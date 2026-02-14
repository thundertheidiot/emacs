;; -*- lexical-binding: t; -*-
(use-package org
  :demand t
  :mode ("\\.org\\'" . org-mode)
  :custom
  (org-src-preserve-indentation t)
  (org-src-tab-acts-natively t)
  (org-startup-with-inline-images t)
  (org-log-done 'time)
  (calendar-week-start-day 1)
  (org-babel-load-languages '((emacs-lisp . t)
			      (shell . t)
			      (eshell . t)
			      (lisp . t)))
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
  :after org
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-roam
  :custom
  (org-roam-directory (file-truename "~/Documents/org/roam"))
  :config
  (unless (file-directory-p org-roam-directory)
    (make-directory org-roam-directory t))
  (setq org-agenda-files (org-roam-list-files))
  (org-roam-db-autosync-mode)
  (org-roam-setup)
  :general
  (:states '(normal visual motion) :keymaps 'override :prefix "SPC"
	   "r" '(:ignore t :wk "roam")
	   "rb" '("buffer" . org-roam-buffer-toggle)
	   "rf" '("find node" . org-roam-node-find)
	   "rI" '("create id" . org-id-get-create)
	   "ri" '("insert node" . org-roam-node-insert)))

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

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

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

(provide 'meow-org)
;;; meow-org.el ends here
