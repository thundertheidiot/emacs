;; -*- lexical-binding: t; -*-
(require 'plz)

(defvar meow/openrouter-data nil
  "Cached data from the openrouter models endpoint.")

(defun meow/openrouter-setup (api-key)
  "Set up openrouter for gptel with the API-KEY."
  (plz 'get "https://openrouter.ai/api/v1/models"
	:headers `(("Authorization" . ,(format "Bearer %s" api-key)))
	:as #'json-read
	:then (lambda (data)
			(let* ((models (mapcar
							(lambda (entry)
							  (let ((pricing (alist-get 'pricing entry)))
								`(,(intern (alist-get 'id entry))
								  :description ,(alist-get 'description entry)
								  :input-cost ,(* 1000000 (string-to-number
														   (alist-get 'prompt pricing)))
								  :output-cost ,(* 1000000 (string-to-number
															(alist-get 'completion pricing)))
								  :context-window ,(/ (alist-get 'context_length entry) 1000)
								  )))
							(alist-get 'data data))))
			  (setq meow/openrouter-data data)
			  (setq gptel-backend
					(gptel-make-openai "OpenRouter"
					  :host "openrouter.ai"
					  :endpoint "/api/v1/chat/completions"
					  :stream t
					  :key api-key
					  :models models))))))

(defun meow/gptel-quick-ask (prompt)
  "Ask PROMPT from the current model in a new gptel buffer."
  (interactive "MAsk: ")
  (let ((buffer (gptel (format-time-string "gptel-%Y%m%d-%H:%M:%S.org")
					   t
					   (format "*** %s" prompt))))
	(if (bound-and-true-p gptel-mode)
		(display-buffer buffer '(display-buffer-same-window))
	  (display-buffer buffer gptel-display-buffer-action)))
  (goto-char (point-max))
  (olivetti-mode -1)
  (visual-line-mode 1))

(defun meow/gptel-openrouter-set-reasoning ()
  "Set reasoning effort for openrouter models."
  (let* ((models (alist-get 'data meow/openrouter-data))
		 (model (seq-find (lambda (m)
							(equal (alist-get 'id m) (symbol-name gptel-model)))
						  models)))
	(when model
	  (let* ((reasoning (alist-get 'reasoning model))
			 (supported-reasoning-efforts (alist-get 'supported_efforts reasoning))
			 (effort (completing-read "Effort level: " (append (mapcar #'identity supported-reasoning-efforts)
															   (when (equal (alist-get 'mandatory reasoning) :json-false)
																 '("none")))  nil t)))
		(when (and effort (> (length effort ) 0))
		  (setq gptel--request-params
				`(:reasoning_effort ,effort)))))))

(advice-add 'gptel--infix-provider :after #'meow/gptel-openrouter-set-reasoning)

(use-package gptel
  :config
  (require 'gptel-autoloads)
  (require 'gptel-context)
  (meow/openrouter-setup
   (with-temp-buffer
	 (insert-file-contents (expand-file-name "openrouterkey" user-emacs-directory))
	 (buffer-string)))
  (setq gptel-model 'deepseek/deepseek-v4-flash-0731
		gptel-default-mode 'org-mode)
  :general-config
  (meow/leader
	"a" '(:ignore t :wk "ai")
	"ao" '("gptel" . gptel)
	"ac" '("ask" . meow/gptel-quick-ask)
	"am" '("menu" . gptel-menu)
	"aa" '("add context" . gptel-context-add)
	"ar" '("remove context" . (lambda () (interactive) (gptel-context-remove)))
	"aR" '("remove all context" . gptel-context-remove-all))
  (:keymaps 'gptel-mode-map :states '(normal)
			"RET" #'gptel-send))

(provide 'meow-ai)
;;; meow-ai.el ends here
