;;; -*- lexical-binding: t; -*-
(require 'web-mode)
(require 'lsp-mode)
(require 'lsp-tailwindcss)

(define-derived-mode vue-mode web-mode "vue")

(defun meow/vue-mode-setup ()
  "Setup for vue mode, run as a hook."
  (setq lsp-tailwindcss-server-path (executable-find "tailwindcss-language-server"))
  (setf (lsp--client-priority (gethash 'ts-ls lsp-clients)) 1)
  (setf (lsp--client-priority (gethash 'vue-semantic-server lsp-clients)) 1)

  (mapc #'lsp-ensure-server '(ts-ls vue-semantic-server eslint))

  (setq-local electric-pair-pairs
			  (append electric-pair-pairs '((?' . ?'))))

  (lsp-deferred))

(setq auto-mode-alist
	  (append  '((".*\\.vue\\'" . vue-mode))
			   auto-mode-alist))

(add-hook 'vue-mode-hook #'meow/vue-mode-setup)

(add-to-list 'apheleia-mode-alist '(vue-mode . eslint))
(add-to-list 'cape-keyword-list '(vue-mode javascript-mode) t)
(evilmi-load-plugin-rules '(vue-mode) '(simple html))

(provide 'lang/meow-vue)
;;; meow-vue.el ends here
