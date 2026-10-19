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

(require 'python)
(setq auto-mode-alist
	  (append '((".*\\.bash\\'" . bash-ts-mode)
				(".*\\.py\\'" . python-ts-mode))
			  auto-mode-alist))

(add-hook 'python-ts-mode-hook #'eglot-ensure)

(use-package json-ts-mode
  :ensure nil
  :mode "\\.json\\'")

(use-package php-mode
  :mode "\\.php\\'"
  :hook (php-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
			   '(php-mode
				 "intelephense" "--stdio")))

;; custom package
(require 'web-mode-tagedit)

(use-package web-mode
  :config
  (define-derived-mode astro-mode web-mode "astro")
  (define-derived-mode bladephp-web-mode web-mode "bladephp")
  (add-hook 'astro-mode-hook #'eglot-ensure)
  (add-hook 'bladephp-web-mode-hook #'eglot-ensure)
  (setq auto-mode-alist
		(append '((".*\\.astro\\'" . astro-mode)
				  (".*\\.blade\\.php\\'" . bladephp-web-mode))
				auto-mode-alist))
  (add-to-list 'eglot-server-programs
               '((astro-mode :language-id "astro")
                 "astro-ls" "--stdio"
                 :initializationOptions
                 (:typescript (:tsdk "./node_modules/typescript/lib"))))
  (add-to-list 'eglot-server-programs
			   '(bladephp-web-mode
				 "rass"
				 "--"
				 "intelephense" "--stdio"
				 "--"
				 "tailwindcss-language-server" "--stdio"))
  :general-config
  (meow/local :keymaps '(web-mode-map vue-mode-map)
	"t" '(:ignore t :wk "tag")
	"ts" '("set" . tagedit-set-attribute)
	"td" '("delete" . tagedit-delete-attribute)))

(use-package emmet-mode
  :hook
  ((web-mode html-mode) . emmet-mode))

(add-hook 'js-mode-hook #'eglot-ensure)

(use-package typescript-ts-mode
  :demand t
  :ensure nil
  :mode (("\\.ts\\'" . typescript-ts-mode)
		 ("\\.tsx\\'" . tsx-ts-mode))
  :hook (typescript-ts-mode . eglot-ensure)
  :hook (tsx-ts-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
			   '((typescript-mode typescript-ts-mode tsx-ts-mode)
				 "rass"
				 "--" "typescript-language-server" "--stdio"
				 "--" "vscode-eslint-language-server" "--stdio"))
  (add-to-list 'apheleia-formatters
			   '(eslint . ("apheleia-npx" "eslint_d" "--fix-to-stdout" "--stdin" "--stdin-filename" filepath)))
  (setq apheleia-formatters-mode-extension-assoc
		(append apheleia-formatters-mode-extension-assoc
				'((tsx-ts-mode . ".tsx")
				  (typescript-ts-mode . ".ts"))))
  (setf
   (alist-get 'typescript-ts-mode apheleia-mode-alist)
   '(eslint))
  (setf
   (alist-get 'tsx-ts-mode apheleia-mode-alist)
   '(eslint)))

(provide 'lang/meow-misc)
;;; meow-misc.el ends here
