;;; -*- lexical-binding: t; -*-
(use-package lua-mode
  :mode "\\.lua\\'"
  :hook (lua-mode . eglot-ensure))

(use-package glsl-mode)

(use-package just-mode)

(use-package sly
  :custom
  (inferior-lisp-program "sbcl"))

(use-package janet-mode
  :mode "\\.janet\\'")

(add-hook 'emacs-lisp-mode-hook #'corfu-mode)

(use-package qml-mode
  :mode "\\.qml\\'")

(use-package php-mode
  :mode "\\.php\\'"
  :hook (php-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
			   '(php-mode
				 "intelephense" "--stdio")))

(defun meow/vue-lsp-setup ()
  (setq lsp-tailwindcss-server-path (executable-find "tailwindcss-language-server"))
  (setf
   (lsp--client-priority (gethash 'ts-ls lsp-clients))
   1)
  (setf
   (lsp--client-priority (gethash 'vue-semantic-server lsp-clients))
   1))

(use-package web-mode
  :config
  (define-derived-mode astro-mode web-mode "astro")
  (define-derived-mode vue-mode web-mode "vue")
  (define-derived-mode bladephp-web-mode web-mode "bladephp")
  (add-hook 'vue-mode-hook
			(lambda ()
			  (meow/vue-lsp-setup)
			  (lsp-deferred)))

  (add-hook 'bladephp-web-mode-hook #'eglot-ensure)
  (setq auto-mode-alist
		(append '((".*\\.astro\\'" . astro-mode)
				  (".*\\.vue\\'" . vue-mode)
				  (".*\\.blade\\.php\\'" . bladephp-web-mode))
				auto-mode-alist))
  (add-to-list 'apheleia-mode-alist '(vue-mode . eslint))
  (add-to-list 'cape-keyword-list '(vue-mode javascript-mode) t)
  (evilmi-load-plugin-rules '(vue-mode) '(simple html))
  (add-hook 'vue-mode #'meow/lsp-supercomplete)
  (add-to-list 'eglot-server-programs
			   '(bladephp-web-mode
				 "rass"
				 "--"
				 "intelephense" "--stdio"
				 "--"
				 "tailwindcss-language-server" "--stdio")))

(use-package emmet-mode
  :hook
  ((web-mode html-mode) . emmet-mode))

(use-package typescript-ts-mode
  :demand t
  :ensure nil
  :mode (("\\.ts\\'" . typescript-ts-mode)
		 ("\\.tsx\\'" . tsx-ts-mode))
  :hook (typescript-ts-mode . eglot-ensure)
  :hook (tsx-ts-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
			   '((typescript-mode tsx-ts-mode astro-mode)
				 "rass"
				 "--" "typescript-language-server" "--stdio"
				 "--" "vscode-eslint-language-server" "--stdio"))
  (add-to-list 'apheleia-formatters
			   '(eslint . ("apheleia-npx" "eslint_d" "--fix-to-stdout" "--stdin" "--stdin-filename" file)))
  (setq apheleia-formatters-mode-extension-assoc
		(append apheleia-formatters-mode-extension-assoc
				'((tsx-ts-mode . ".tsx")
				  (typescript-ts-mode ".ts"))))
  (setf
   (alist-get 'typescript-ts-mode apheleia-mode-alist)
   '(eslint))
  (setf
   (alist-get 'tsx-ts-mode apheleia-mode-alist)
   '(eslint)))

(provide 'lang/meow-misc)
;;; meow-misc.el ends here
