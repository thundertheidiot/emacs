;; -*- lexical-binding: t; -*-
(require 'meow-helpers)
(require 's)

(use-package fish-completion)

(defun meow/eshell (&optional projectile &rest args)
  (if projectile
      (projectile-run-eshell t)
    (eshell t))
  (goto-char (point-max))
  (evil-append-line 1))

(defvar-local meow/eshell-nix-shell-environment nil
  "Store environment for nix shell.")
(defvar-local meow/eshell-nix-shell-path nil
  "Store path for nix shell.")

(defun meow/eshell-prompt ()
  "Custom eshell prompt."
  (concat
   (when (and meow/eshell-nix-shell-path meow/eshell-nix-shell-environment)
     (propertize "<nix-shell> " 'face '(:foreground "green")))

   (abbreviate-file-name (eshell/pwd))

   ;; (if (and (not (file-remote-p default-directory)) (magit-toplevel))
   ;;     (propertize (format "  %s" (magit-get-current-branch)) 'face '(:foreground "#cba6f7"))
   ;;   "")
   
   (propertize " λ" 'face
	       (if (string-match (rx
				  "/sudo:root"
				  (* nonl))
				 (eshell/pwd))
		   '(:foreground "red")
		 '(:foreground "purple")))
   (propertize " " 'face
	       'default)))

(use-package eat
  :demand t
  :hook
  (eat-mode . meow/turn-off-line-numbers)
  :config
  (eat-eshell-mode))

(use-package eshell
  :ensure nil
  :demand t ;; force instant load
  :after (magit fish-completion)
  :commands (eshell projectile-run-eshell)
  :custom
  (eshell-history-size 10000000)
  (eshell-prompt-function #'meow/eshell-prompt)
  (eshell-prompt-regexp
   (rx line-start
       (*?
	nonl)
       "λ "))
  (eshell-visual-commands '())
  :config
  (add-to-list 'eshell-modules-list 'eshell-elecslash)
  (add-to-list 'eshell-modules-list 'eshell-tramp)
  :hook
  (eshell-mode . meow/turn-off-line-numbers)
  (eshell-mode . fish-completion-mode)
  :general-config
  (:states '(normal visual) :keymaps 'eshell-mode-map
	   "A" (lambda () (interactive) (end-of-buffer) (evil-append-line 1)))
  (:states '(normal visual insert) :keymaps 'eshell-mode-map
	   "C->" (lambda () (interactive) 
		   (insert (concat "> #<buffer " (read-buffer "Send to: ") ">")))
	   "C-p" (lambda () (interactive)
		   (insert (read-file-name "Insert path: "))))
  (:keymaps 'eshell-mode-map :states '(normal visual motion)
	    "RET" (lambda () (interactive)
		    (unless (ignore-errors (browse-url))
		      (evil-ret))))
  :general
  (:states '(normal visual insert emacs motion) :prefix "SPC" :keymaps 'override :global-prefix "C-SPC"
	   "oe" '("eshell" . (lambda () (interactive) 
			       (select-window (meow/intelligent-split t)) 
			       (meow/eshell)))
	   "oE" '("eshell in this window" . (lambda () (interactive) (meow/eshell)))
	   "poe" '("eshell" . (lambda () (interactive) 
				(select-window (meow/intelligent-split t))
				(meow/eshell t)))
	   "poE" '("eshell in this window" . (lambda () (interactive) (meow/eshell t)))))

(defun eshell/v (&rest args)
  "Exec visual command ARGS in a new window."
  (select-window (meow/intelligent-split t))
  (apply 'eshell-exec-visual args))

(defalias 'eshell/V 'eshell-exec-visual)

(defun eshell/c ()
  (eshell/cd
   (read-file-name "Change directory: ")))

(defun meow/eshell-apply-nix-shell ()
  "Apply the set nix shell environment for eshell."
  (when (and meow/eshell-nix-shell-path meow/eshell-nix-shell-environment)
    (let ((path (eshell-get-path t)))
      ;; other things (e.g. envrc.el) may alter the pathas well
      (eshell-set-path (append path meow/eshell-nix-shell-path)))
    (mapcar (lambda (e) (ignore-errors (eshell-set-variable (car e) (cadr e))))
	    meow/eshell-nix-shell-environment))
  nil)

(add-hook 'eshell-directory-change-hook #'meow/eshell-apply-nix-shell)

;; (defun meow/eshell-nix-command-not-found (orig-fun &rest args)
;;   (if (executable-find "command-not-found")
;;       (condition-case err
;; 	  (apply orig-fun args)
;; 	(error (if (s-contains-p "command not found" (error-message-string err))
;; 		   (eshell-connection-local-command
;; 		    "command-not-found"
;; 		    (car args)
;; 		    ;; (eshell-command-result (format "command-not-found %s"
;; 		    ;; 				   (car args)))
;; 		    )
;; 		 (eval err))))
;;     (apply orig-fun args)))

;; (advice-add 'eshell-find-interpreter :around #'meow/eshell-nix-command-not-found)

(defun eshell/ns (&rest args)
  "Nix shell helper for eshell, ARGS are given to nix shell."
  (let ((path (eshell-get-path))
	(env process-environment)
	(packages (mapcar
		   (lambda (p) (if (s-contains-p "#" p)
				   p
				 (format "nixpkgs#%s" p)))
		   args)))

    (let* ((output (shell-command-to-string
		    (format "nix shell %s --command env"
			    (mapconcat #'identity packages " "))))
	   (lines (split-string output "\n" t))
	   (environment (mapcar (lambda (line)
				  (s-split-up-to "=" line 1))
				lines)))
      (let ((list '()))
	(dolist (env environment)
	  (when (= 2 (length env))
	    (if (string= (car env) "PATH")
		(setq meow/eshell-nix-shell-path (split-string (cadr env) ":" t))
	      (push env list))))
	(setq meow/eshell-nix-shell-environment list))

      (meow/eshell-apply-nix-shell))))

(use-package pcre2el)
(defmacro re (&rest rx-sexp) ;; Stolen from https://youtube.com/watch?v=9xLeqwl_7n0
  "Convert rx expression RX-SEXP to pcre compatible regexp."
  `(rxt-elisp-to-pcre (rx ,@rx-sexp)))

(defalias 'eshell/less 'view-file)

;; exit closes window
(defun eshell/exit ()
  "Exit the nix shell environment if we are in one.
Otherwise exit eshell and close the window with `evil-quit'."
  (if (and meow/eshell-nix-shell-path meow/eshell-nix-shell-environment)
      (progn
	(setq meow/eshell-nix-shell-path nil
	      meow/eshell-nix-shell-environment nil)
	;; weird reset hack TODO get a better way
	(cl-letf (((symbol-function 'eshell-add-to-dir-ring) #'ignore))
	  (eshell/cd ".")))
    (progn
      (evil-quit)
      (throw 'eshell-terminal t))))

(defalias 'eshell/e 'eshell/exit)

;; vterm
(defun meow/vterm (&optional projectile)
  (if projectile
      (projectile-run-vterm t)
    (vterm t))
  (end-of-buffer)
  (evil-append-line 1))

(defun vterm-evil-insert ()
  "Mimic evil i in vterm."
  (interactive)
  (vterm-goto-char (point))
  (call-interactively #'evil-insert))

(defun vterm-evil-append ()
  "Mimic evil a in vterm."
  (interactive)
  (vterm-goto-char (1+ (point)))
  (call-interactively #'evil-append))

(defun vterm-evil-append-line ()
  "Mimic A in vterm."
  (interactive)
  (vterm-goto-char (point-max))
  (call-interactively #'evil-insert))

(defun vterm-evil-delete ()
  "Provide similar behavior as `evil-delete'."
  (interactive)
  (let ((inhibit-read-only t))
    (cl-letf (((symbol-function #'delete-region) #'vterm-delete-region))
      (call-interactively 'evil-delete))))

(defun vterm-evil-change ()
  "Provide similar behavior as `evil-change'."
  (interactive)
  (let ((inhibit-read-only t))
    (cl-letf (((symbol-function #'delete-region) #'vterm-delete-region))
      (call-interactively 'evil-change))))

(use-package vterm
  :hook (vterm-mode . meow/turn-off-line-numbers)
  :commands (vterm)
  :custom
  (vterm-kill-buffer-on-exit t)
  (vterm-max-scrollback 100000)
  ;; :config
  ;; (add-hook 'vterm-exit-functions #'meow/vterm-process-finished)
  :general
  (:states '(normal visual motion) :keymaps 'override :prefix "SPC"
	   "ov" '((lambda () (interactive)
		    (select-window (meow/intelligent-split t))
		    (meow/vterm)) :wk "vterm")
	   "oV" '((lambda () (interactive)
		    (meow/vterm)) :wk "vterm in this window")
	   "pov" '((lambda () (interactive)
		     (select-window (meow/intelligent-split t))
		     (meow/vterm t)) :wk "vterm")
	   "poV" '((lambda () (interactive)
		     (meow/vterm t)) :wk "vterm in this window"))
  :general-config
  (:states '(normal visual) :keymaps 'vterm-mode-map
	   "a" 'vterm-evil-append
	   "A" 'vterm-evil-append-line
	   "d" 'vterm-evil-delete
	   "i" 'vterm-evil-insert
	   "c" 'vterm-evil-change))

;; https://github.com/akermu/emacs-libvterm/issues/313#issuecomment-1183650463
(advice-add #'vterm--redraw :around (lambda (fun &rest args) (let ((cursor-type cursor-type)) (apply fun args))))

;; eshell visual exec in vterm
(use-package eshell-vterm
  :demand t
  :after eshell
  :config 
  (eshell-vterm-mode))

(provide 'meow-terminal)
;;; meow-terminal.el ends here
