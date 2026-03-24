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

(use-package typescript-ts-mode
  :demand t
  :ensure nil
  :mode (("\\.ts\\'" . typescript-ts-mode)
		 ("\\.tsx\\'" . tsx-ts-mode))
  :hook (typescript-ts-mode . eglot-ensure)
  :hook (tsx-ts-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
			   '((typescript-mode tsx-ts-mode)
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
