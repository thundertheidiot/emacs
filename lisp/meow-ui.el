;; -*- lexical-binding: t; -*-
(require 'meow-helpers)

;; disable some useless default features
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)
(global-visual-line-mode)

(add-hook 'emacs-lisp-mode-hook #'prettify-symbols-mode)


(setq scroll-conservatively 10)

(use-package vertico
  :demand t
  :custom
  (vertico-resize t)
  (vertico-cycle nil)
  :config
  (require 'vertico-autoloads)
  (vertico-mode)
  (vertico-buffer-mode)
  :general-config
  (:keymaps 'vertico-map :states '(normal visual)
			"j" #'vertico-next
			"k" #'vertico-previous
			"gg" #'vertico-first
			"G" #'vertico-last)
  (:keymaps 'vertico-map :states '(normal visual insert)
			"RET" #'vertico-exit
			"C-u" #'vertico-quick-exit
			"C-j" #'vertico-next
			"C-k" #'vertico-previous
			"C-l" #'vertico-quick-jump)
  (:keymaps 'vertico-map :states '(insert)
			"<backspace>" #'vertico-directory-delete-char
			"DEL" #'vertico-directory-delete-char)
  (:keymaps 'override :states '(normal visual insert)
			"C-c c" #'vertico-buffer-mode
			"C-c s" #'vertico-suspend))

(use-package vertico-posframe
  :after vertico
  :hook
  (minibuffer-setup . (lambda ()
						(when vertico-posframe-mode
						  (setq-local vertico-posframe-height 35
									  vertico-count 35))))
  :general-config
  (:keymaps 'override :states '(normal visual insert)
			"C-c p" #'vertico-posframe-mode))

(use-package vertico-prescient
  :after vertico
  :custom
  (vertico-prescient-enable-filtering nil)
  :config
  (vertico-prescient-mode +1))

(use-package consult
  :custom
  (consult-line-start-from-top nil)
  :config
  (require 'consult-autoloads)
  :general-config
  (meow/leader
    "/" '("line search" . consult-line)
    "sg" '("grep" . (lambda () (interactive)
					  (consult-ripgrep (expand-file-name ""))))
    "f" '("recent file" . consult-recent-file)
    "sf" '("find" . consult-fd)
    "si" '("imenu" . consult-imenu)
    "bs" '("switch" . consult-buffer)
    "bs" '("switch" . consult-buffer)
    "bo" '("open buffer in new window" . (lambda () (interactive)
										   (select-window (meow/intelligent-split t))
										   (consult-buffer)))))

(defun advice!-consult-grep-evil-search-history (ret)
  "Add the selected item to the evil search history."
  (when ret ;; return value is nil if you quit early
    (let ((search (if (string= (substring (car consult--grep-history) 0 1) "#")
					  (substring (car consult--grep-history) 1 nil)
					(car consult--grep-history))))
      (add-to-history 'regexp-search-ring search)
      (add-to-history 'evil-ex-search-history search)
      (setq evil-ex-search-pattern (list search t t))
      (setq evil-ex-search-direction 'forward))
    ret))
(advice-add 'consult--grep :filter-return #'advice!-consult-grep-evil-search-history)

(defun advice!-consult-line-evil-search-history (ret)
  "Add the selected item to the evil search history."
  (when ret ;; return value is nil if you quit early
    (let ((search (car consult--line-history)))
      (add-to-history 'regexp-search-ring search)
      (add-to-history 'evil-ex-search-history search)
      (setq evil-ex-search-pattern (list search t t))
      (setq evil-ex-search-direction 'forward))
    ret))
(advice-add 'consult-line :filter-return #'advice!-consult-line-evil-search-history)

(use-package wgrep)
(use-package embark
  ;; :after wgrep
  :demand t
  :general-config
  (:keymaps 'vertico-map :states '(normal visual insert)
			"C-;" (lambda () (interactive)
					(if embark--selection
						(embark-act-all)
					  (embark-act)))
			"M-a" (lambda () (interactive)
					(embark-select)
					(vertico-next)))
  (:keymaps 'vertico-map :states '(normal visual)
			"m" (lambda () (interactive)
				  (embark-select)
				  (vertico-next))))

(use-package embark-consult
  :after embark
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package orderless
  :demand t
  :after (vertico consult)
  :custom
  (completion-styles '(orderless basic))
  (orderless-matching-styles '(orderless-literal orderless-prefixes orderless-regexp))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :demand t
  :config
  (marginalia-mode))

(defun advice!-crm-indicator (args)
  (cons (format "[CRM%s] %s"
				(replace-regexp-in-string
				 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
				 crm-separator)
				(car args))
		(cdr args)))
(advice-add #'completing-read-multiple :filter-args #'advice!-crm-indicator)

(setq minibuffer-prompt-properties '(read-only t cursor-intangible-mode t face minibuffer-prompt)
      enable-recursive-minibuffers t)

(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; keybinding helper
(use-package which-key
  :demand t
  :config
  (which-key-setup-side-window-bottom)
  (which-key-mode))

;; icons for corfu
(use-package kind-icon
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package ligature
  :config
  (ligature-set-ligatures 'prog-mode
						  '("==" "===" "!=" "!==" "&&" "||"))
  (global-ligature-mode t))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode)
  (org-mode . rainbow-delimiters-mode))


;; icons
(use-package all-the-icons)

;; icons for dired
(use-package all-the-icons-dired
  :after all-the-icons
  :hook (dired-mode . (lambda ()
						(when (display-graphic-p)
						  (all-the-icons-dired-mode)))))

;; icons for ibuffer
(use-package all-the-icons-ibuffer
  :after all-the-icons
  :hook (ibuffer-mode . (lambda ()
						  (when (display-graphic-p)
							(all-the-icons-ibuffer-mode)))))

;; actually parse colors for everything
(use-package xterm-color
  :demand t
  :after eshell
  :hook (comint-mode . (lambda ()
						 (setq-local comint-output-filter-functions
									 (remove 'ansi-color-process-output comint-output-filter-functions))
						 (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))
  :hook (eshell-before-prompt . (lambda ()
								  (setq xterm-color-preserve-properties t)))
  :hook (eshell-mode . (lambda ()
						 (setenv "TERM" "xterm-256color")))
  :hook (shell-mode . (lambda ()
						;; Disable font-locking in this buffer to improve performance
						(font-lock-mode -1)
						;; Prevent font-locking from being re-enabled in this buffer
						(make-local-variable 'font-lock-function)
						(setq font-lock-function (lambda (_) nil))
						;; Replace ansi-color-process-output with xterm-color-filter
						(make-local-variable 'comint-output-filter-functions)
						(remove-hook 'comint-output-filter-functions 'ansi-color-process-output t)
						(add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))
  :config
  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
  (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions)))

(use-package dashboard
  :custom
  (dashboard-projects-backend 'projectile)
  (dashboard-projects-switch-function #'projectile-switch-project-by-name)
  (dashboard-items '((projects . 5)
					 (recents . 5)
					 (agenda . 20)))
  (dashboard-filter-agenda-entry
   (lambda ()
     (if (member "lukujärjestys" (org-get-tags))
		 (point)
       (dashboard-filter-agenda-by-time))))
  (dashboard-agenda-tags-format #'ignore)
  :hook
  (dashboard-mode . (lambda ()
					  (display-line-numbers-mode -1)))
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice #'dashboard-open))

(provide 'meow-ui)
;;; meow-ui.el ends here
