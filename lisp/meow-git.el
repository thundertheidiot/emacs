;; -*- lexical-binding: t; -*-
(use-package magit
  :demand t
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (magit-auto-revert-mode nil)
  :general-config
  (:states '(normal visual motion) :keymaps 'override :prefix "SPC"
	   "g" '(:ignore t :wk "git")
	   "gg" '("open magit" . magit-status)
	   "gd" '(:ignore t :wk "diff")
	   "gdu" '("diff unstaged" . magit-diff-unstaged)
	   "gds" '("diff staged" . magit-diff-staged)
	   "gc" '("commit" . magit-commit)
	   "gp" '("push" . magit-push)
	   "gF" '("push" . magit-pull)))

(use-package magit-todos
  :after magit
  :hook (magit-mode . magit-todos-mode)
  :config (magit-todos-mode 1))

(defun meow/last-diff-hl-hunk (&optional backward)
  "Go to the last hunk in the file, first if BACKWARD is t."
  (while-let ((pos (diff-hl-search-next-hunk backward)))
    (goto-char (overlay-start pos))))

(defun advice!diff-hl-next-hunk-loop-around (orig-fun &rest args)
  (let ((backward (if (car args)
		      nil
		    t)) ;; flip
	(return (ignore-errors (funcall orig-fun args))))
    (unless return
      (meow/last-diff-hl-hunk backward)
      (message "Looped around"))))

(use-package diff-hl
  :demand t
  :custom
  (diff-hl-show-staged-changes nil)
  (diff-hl-global-modes '(not image-mode pdf-view-mode))
  (diff-hl-update-async t)
  (vc-git-diff-switches '("--histogram"))
  :config
  (require 'diff-hl-autoloads)
  (require 'diff-hl-dired)
  (require 'diff-hl-margin)
  ;; (require 'diff-hl-flydiff) ;; breaks on igc
  (advice-add 'diff-hl-next-hunk :around #'advice!diff-hl-next-hunk-loop-around)
  (global-diff-hl-mode +1)
  :hook
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh)
  ;; (dired-mode . diff-hl-dired-mode)
  ;; (diff-hl-mode . diff-hl-flydiff-mode) ;; breaks on igc right now
  (diff-hl-mode . diff-hl-margin-mode) ;; to simultaniously support flycheck symbols in fringe
  :general
  (:states '(normal visual motion) :keymaps 'override :prefix "SPC"
	   "ga" '("stage hunk" . diff-hl-stage-current-hunk)
	   "gr" '("revert hunk" . diff-hl-revert-hunk)
	   "gn" '("next hunk" . diff-hl-next-hunk)
	   "gN" '("previous hunk" . diff-hl-previous-hunk)))

(use-package git-timemachine
  :general-config
  (:states 'normal :keymaps 'git-timemachine-mode-map
	   "<" 'git-timemachine-show-previous-revision
	   "J" 'git-timemachine-show-previous-revision
	   ">" 'git-timemachine-show-next-revision
	   "K" 'git-timemachine-show-next-revision
	   "i" nil ;; no point in going to insert mode, the buffer is read only
	   "C-f" (lambda () (interactive) (git-timemachine-show-nth-revision 1))
	   "C-g" 'git-timemachine-show-nth-revision
	   "C-c" 'git-timemachine-show-current-revision)
  :general
  (:states '(normal visual motion) :keymaps 'override :prefix "SPC"
	   "gt" '("timemachine" . git-timemachine-toggle)))

(require 'emsg-blame)
(global-emsg-blame-mode t)

(provide 'meow-git)
;;; meow-git.el ends here
