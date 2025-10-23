(require 'meow/helpers)

(use-package vertico
  :custom
  (vertico-resize t)
  (vertico-cycle nil)
  :config
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

(provide 'meow/ui)
