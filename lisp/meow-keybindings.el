;; -*- lexical-binding: t; -*-
(require 'meow-helpers)
(require 'meow-window-configuration)

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :custom
  (evil-want-keybinding nil)
  (evil-want-integration t)
  (evil-want-minibuffer t)
  (evil-vsplit-window-right t)
  (evil-split-window-below t)
  (evil-undo-system 'undo-tree)
  :config
  (evil-set-undo-system evil-undo-system)
  (evil-set-initial-state 'minibuffer-mode 'insert)
  (evil-set-initial-state 'minibuffer-inactive-mode 'insert)
  (evil-mode))

(use-package evil-collection
  :after evil
  :custom
  (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init '(apropos
			  calc
			  compile
			  comint
			  dashboard
			  debug
			  ediff
			  emoji
			  eshell
			  woman
			  pdf
			  org
			  proced
			  dired
			  elfeed
			  wdired
			  image
			  ibuffer
			  simple-mpc
			  magit
			  forge
			  magit-todos
			  vdiff
			  sly
			  wgrep
			  yaml-mode
			  diff-hl
			  vterm
			  eat)))

(use-package evil-better-visual-line
  :demand t
  :after evil
  :config
  (evil-better-visual-line-on))

(use-package evil-textobj-tree-sitter
  :after evil
  :config
  (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
  (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner")))

(use-package general
  :demand t
  :config
  (general-evil-setup))

(general-create-definer meow/leader
  :states '(normal insert visual emacs motion)
  :keymaps 'override
  :prefix "SPC"
  :global-prefix "C-SPC")

(general-create-definer meow/local
  :states '(normal insert visual emacs motion)
  :prefix "SPC l"
  :global-prefix "C-SPC l")

;; basic keybindings
(meow/leader
  "w" '(:ignore t :wk "window")
  "wh" '("move left" . windmove-left)
  "wj" '("move down" . windmove-down)
  "wk" '("move up" . windmove-up)
  "wl" '("move right" . windmove-right)
  "<left>" '("move left" . windmove-left)
  "<down>" '("move down" . windmove-down)
  "<up>" '("move up" . windmove-up)
  "<right>" '("move right" . windmove-right)
  "wq" '("close" . evil-quit)
  "ww" '("close" . evil-quit)
  "ws" '("horizontal split" . (lambda () (interactive) (select-window (meow/intelligent-split t))))

  "wc" '(:ignore t :wk "window configurations")
  "wcl" '("load" . meow/load-window-configuration)
  "wcs" '("save" . meow/save-window-configuration)
  "wcd" '("delete" . meow/delete-window-configuration)
  "wcn" '("new" . meow/new-window-configuration)
  
  "H" '("increase window width" . (lambda () (interactive) (evil-window-increase-width 2)))
  "J" '("increase window height" . (lambda () (interactive) (evil-window-increase-height 2)))
  
  "l" '(:ignore t :wk "local (mode specific)")
  "s" '(:ignore t :wk "search")

  "d" '("dired" . (lambda () (interactive)
                    (when default-directory
                      (select-window (meow/intelligent-split t))
                      (dired default-directory))))

  "D" '("dired in current window" . (lambda () (interactive)
				      (when default-directory
					(dired default-directory))))

  "o" '(:ignore t :wk "open")
  "oc" '("world clock" . world-clock)

  ":" '("M-x" . execute-extended-command)
  ";" '("M-x" . execute-extended-command)
  "." '("find file" . find-file)
  ">" '("find file from ~/" . (lambda () (interactive) (find-file (getenv "HOME"))))
  
  "h" '(:ignore t :wk "help")
  "hb" '("describe binding" . describe-bindings)
  "hf" '("describe function" . describe-function)
  "hv" '("describe variable" . describe-variable)
  "hF" '("describe face" . describe-face)
  "hk" '("describe key" . describe-key)
  "hs" '("find function" . find-function)
  "ha" '("describe" . apropos)
  
  "b" '(:ignore t :wk "buffer")
  "bi" '("ibuffer" . ibuffer)
  "bK" '("kill buffer" . kill-buffer)
  "bk" '("kill this buffer" . kill-current-buffer)

  "e" '(:ignore t :wk "emacs")
  "ec" '("async shell command" . async-shell-command)
  "er" '("eval region or line" . meow/eval-region-and-go-to-normal-mode)
  "eb" '("eval buffer" . eval-buffer)
  "ee" '("eval expression" . eval-expression)
  "ei" '("eval & insert" . (lambda () (interactive)
			     (insert (format "%s"
					     (eval (read--expression "E&I: "))))))
  "eI" '("e&i expr+value" . (lambda () (interactive)
			      (insert (let ((expr (read--expression "E&I (expr): ")))
					(format "%s = %s"
						expr
						(eval expr)))))))

(general-define-key
 :states '(normal visual)
 "gc" 'meow/comment-or-uncomment-region-or-line
 "<up>" 'enlarge-window
 "<left>" 'shrink-window-horizontally
 "<right>" 'enlarge-window-horizontally
 "<down>" 'shrink-window
 ";" 'evil-ex
 "M-k" 'enlarge-window
 "M-h" 'shrink-window-horizontally
 "M-l" 'enlarge-window-horizontally
 "M-j" 'shrink-window

 "C-j" #'backward-sexp
 "C-k" #'forward-sexp
 "C-d" #'kill-sexp)

(general-define-key
 "C-=" 'text-scale-increase
 "C--" 'text-scale-decrease
 "C-j" nil
 "<escape>" #'keyboard-quit
 "<escape>" #'keyboard-escape-quit
 "ESC" #'keyboard-quit
 "ESC" #'keyboard-escape-quit
 "<C-wheel-up>" 'text-scale-increase
 "<C-wheel-down>" 'text-scale-decrease)

(general-def :keymaps 'override
  "M-x" 'execute-extended-command)


(provide 'meow-keybindings)
;;; meow-keybindings.el ends here
