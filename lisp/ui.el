;; -*- lexical-binding: t; -*-
(require 'meow/helpers "helpers")

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode)
(global-visual-line-mode)

(add-hook 'emacs-lisp-mode-hook #'prettify-symbols-mode)

(use-package vertico
  :custom
  (vertico-resize t)
  (vertico-cycle nil)
  :config
  (require 'vertico-directory)
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

(use-package corfu
  :demand t
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.18)
  (corfu-cycle t)
  :hook
  (after-init . global-corfu-mode)
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
	    "M-i" (lambda () (interactive)
		    (let ((current-prefix-arg t))
		      (call-interactively #'corfu-info-documentation)))
	    "C-j" #'corfu-next
	    "C-k" #'corfu-previous
	    "S-RET" #'corfu-complete
	    "S-<return>" #'corfu-complete))

(provide 'meow/ui)
;;; ui.el ends here
