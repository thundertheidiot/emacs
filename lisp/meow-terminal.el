;; -*- lexical-binding: t; -*-
(require 'meow-helpers)

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

;; (defun meow/vterm-process-finished (buffer _event)
;;   (when (buffer-live-p buffer)
;;     (let ((inhibit-read-only t)
;; 	  (content (buffer-string)))
;;       (goto-char (point-max))

;;       (kill-local-variable 'change-major-mode-hook)
;;       ;; (message content)
;;       (fundamental-mode)
;;       ;; (erase-buffer)
;;       ;; (insert content)

;;       ;; clean up white space
;;       (skip-chars-backward "\n\r\t ")
;;       (delete-region (point) (point-max))

;;       (insert "\n\n")
;;       (insert (propertize "Process finished." 'face
;; 			  `(:foreground ,(face-attribute 'font-lock-keyword-face :foreground)
;; 					:weight bold
;; 					:height 1.2)))
;;       (goto-char (point-max))

;;       (when-let* ((window (get-buffer-window buffer)))
;; 	(set-window-point window (point-max)))
;;       )))

(use-package vterm
  :hook (vterm-mode . meow/turn-off-line-numbers)
  :commands (vterm)
  :custom
  (vterm-kill-buffer-on-exit nil)
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

(use-package eshell-vterm
  :demand t
  :after eshell
  :config 
  (eshell-vterm-mode))

(use-package fish-completion)

(defun meow/eshell (&optional projectile &rest args)
  (if projectile
      (projectile-run-eshell t)
    (eshell t))
  (end-of-buffer)
  (evil-append-line 1))

;; eshell prompt
(defvar-local eshell-nix-shell-active nil
  "Show <nix-shell> in the eshell prompt.")

(defun meow/eshell-prompt ()
  "Custom eshell prompt."
  (concat
   (if eshell-nix-shell-active
       (propertize "<nix-shell> " 'face '(:foreground "green"))
     "")
   (abbreviate-file-name (eshell/pwd))
   
   (if (magit-toplevel)
       (propertize (format "  %s" (magit-get-current-branch)) 'face '(:foreground "#cba6f7"))
     "")

   (propertize " λ" 'face
	       (if (string-match (rx
				  "/sudo:root"
				  (* nonl))
				 (eshell/pwd))
		   '(:foreground "red")
		 '(:foreground "purple")))
   (propertize " " 'face
	       'default)))

(use-package eshell
  :ensure nil
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
  :config
  (add-to-list 'eshell-modules-list 'eshell-elecslash)
  (add-to-list 'eshell-modules-list 'eshell-tramp)

  (setq eshell-visual-commands '(
				 "nix"
				 "nix-build"
				 "nixos-rebuild"
				 "nh"
				 "deploy"
				 "btop"
				 "htop"))
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

(defun eshell/nix-shell (&rest args)
  (if (member "--run" args)
      (eshell-command-result
       (concat "*nix-shell " (mapconcat 'identity args " ")))
    (let* ((output (shell-command-to-string
		    (format "nix-shell %s --run \"env\""
			    (mapconcat 'identity args " "))))
	   (lines (split-string output "\n" t))
	   (environment (mapcar (lambda (line)
				  (s-split-up-to "=" line 1))
				lines)))
      (dolist (env environment)
	(when (= 2 (length env))
	  (if (string= (car env) "PATH")
	      (eshell-set-path (cadr env))
	    (ignore-errors
	      (eshell-set-variable (car env) (cadr env))))
	  
	  (setq-local eshell-nix-shell-active t))))))

(use-package pcre2el)
(defmacro re (&rest rx-sexp) ;; Stolen from https://youtube.com/watch?v=9xLeqwl_7n0
  "Convert rx expression RX-SEXP to pcre compatible regexp."
  `(rxt-elisp-to-pcre (rx ,@rx-sexp)))

(defalias 'eshell/less 'view-file)

;; exit closes window
(defun eshell/exit ()
  (evil-quit)
  (throw 'eshell-terminal t))

(defalias 'eshell/e 'eshell/exit)

(provide 'meow-terminal)
;;; meow-terminal.el ends here
