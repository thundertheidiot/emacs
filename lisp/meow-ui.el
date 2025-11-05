;; -*- lexical-binding: t; -*-
(require 'meow-helpers)

;; disable some useless default features
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)
(global-visual-line-mode)

(add-hook 'emacs-lisp-mode-hook #'prettify-symbols-mode)

;; fonts
(defun meow/set-fonts ()
  (set-face-attribute 'default nil
		      :family "Monospace"
		      :height 110
		      :weight 'regular)

  (set-face-attribute 'variable-pitch nil
		      :font "Sans-Serif"
		      :height 120
		      :weight 'medium)

  (set-face-attribute 'fixed-pitch nil
		      :font "Monospace"
		      :weight 'medium)

  (set-face-attribute 'font-lock-comment-face nil
		      :slant 'italic)
  (set-face-attribute 'font-lock-keyword-face nil
		      :slant 'italic)

  (add-to-list 'default-frame-alist '(font . "Monospace")))

(add-hook 'after-init-hook #'meow/set-fonts)
(add-hook 'server-after-make-frame-hook #'meow/set-fonts)

(setq-default line-spacing 0.12)

(setq scroll-conservatively 10)
(setq scroll-margin 7)

(use-package vertico
  :custom
  (vertico-resize t)
  (vertico-cycle nil)
  :config
  (require 'vertico-autoloads)
  (vertico-mode)
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
	    "C-c c" #'vertico-buffer-mode))

(use-package consult
  :custom
  (consult-line-start-from-top nil)
  :config
  (require 'consult-autoloads)
  :general-config
  (meow/leader
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
  ("C-;" #'embark-act
   "C-a" #'embark-select))

(use-package embark-consult
  :after embark
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package orderless
  :demand t
  :after (vertico consult)
  :custom
  (completion-styles '(orderless basic))
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

;; theme
(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :hook
  (after-init . (lambda ()
		  (load-theme 'doom-Iosvkem t)))
  (server-after-make-frame . (lambda ()
			       (load-theme 'doom-Iosvkem t)))
  :config
  (require 'doom-themes-autoloads)
  (doom-themes-org-config))

;; color frames differently
(use-package solaire-mode
  :hook
  (after-init . (lambda ()
		  (when (display-graphic-p)
		    (solaire-global-mode +1))))
  (server-after-make-frame . (lambda ()
			       (when (display-graphic-p)
				 (solaire-global-mode +1)))))

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

(provide 'meow-ui)
;;; meow-ui.el ends here
