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
							  (let ((pricing (alist-get 'pricing entry))
									(parameters (alist-get 'supported_parameters entry)))
								`(,(intern (alist-get 'id entry))
								  :description ,(alist-get 'description entry)
								  :input-cost ,(* 1000000 (string-to-number
														   (alist-get 'prompt pricing)))
								  :output-cost ,(* 1000000 (string-to-number
															(alist-get 'completion pricing)))
								  :context-window ,(/ (alist-get 'context_length entry) 1000)
								  :capabilities ,(seq-keep #'identity `(,(when (seq-find (lambda (e) (string= "tools" e)) parameters) 'tool-use)
																		,(when (seq-find (lambda (e) (string= "reasoning" e)) parameters) 'reasoning)
																		,(when (seq-find (lambda (e) (string= "structured_outputs" e)) parameters) 'json))) 
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
  (visual-line-mode 1)
  (gptel-send))

(defun meow/gptel-openrouter-set-reasoning ()
  "Set reasoning effort for openrouter models.
Called as an advice after selecting a model from the menu."
  (if-let* ((_check (string= "OpenRouter" (gptel-backend-name gptel-backend)))
			(models (alist-get 'data meow/openrouter-data))
			(model (seq-find (lambda (m)
							   (equal (alist-get 'id m) (symbol-name gptel-model)))
							 models))
			(reasoning (alist-get 'reasoning model))
			(supported-reasoning-efforts (alist-get 'supported_efforts reasoning))
			(effort (completing-read "Effort level: " (append (mapcar #'identity supported-reasoning-efforts)
															  (when (equal (alist-get 'mandatory reasoning) :json-false)
																'("none")))
									 nil 'require-match))
			(_check (> (length effort) 0)))
	  (setq gptel--request-params
			`(:reasoning_effort ,effort))
	(setq gptel--request-params '())))

(advice-add 'gptel--infix-provider :after #'meow/gptel-openrouter-set-reasoning)

(use-package gptel
  :config
  (require 'gptel-autoloads)
  (require 'gptel-context)
  (meow/openrouter-setup
   (with-temp-buffer
	 (insert-file-contents (expand-file-name "openrouterkey" user-emacs-directory))
	 (buffer-string)))
  (setq gptel-default-mode 'org-mode
		gptel-model 'deepseek/deepseek-v4-flash-0731
		gptel--request-params '(:reasoning_effort "low"))
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

(use-package gptel-zai)

;; tools

(defvar meow/gptel-tool-search
  (gptel-make-tool
   :function (lambda (callback query)
			   (let ((url (format "http://127.0.0.1:8080/search?q=%s&format=json"
								  (url-hexify-string query))))
				 (url-retrieve url
							   (lambda (_status)
								 (goto-char (point-min))
								 (search-forward "\n\n") ;; end of http headers
								 (let ((json-response (json-read)))
								   (funcall callback
											(mapconcat (lambda (result)
														 (format "%s - %s\n%s"
																 (cdr (assoc 'title result))
																 (cdr (assoc 'url result))
																 (cdr (assoc 'content result))))
													   (cdr (assoc 'results json-response))
													   "\n\n")))))))
   :async t
   :name "search_web"
   :description "Searches the web and returns formatted results including titles, URLs, and content excerpts."
   :args (list
		  '(:name "query"
				  :type string
				  :description "The search query to execute against the search engine."))
   :category "web"
   :include t))

(defvar meow/gptel-tool-fetch-url
  (gptel-make-tool
   :function (lambda (callback url)
			   (let* ((output-buffer (generate-new-buffer (format " *trafilatura-%s* " url)))
					  (proc (start-process "trafilatura-process"
										   output-buffer
										   "trafilatura" "-u" url)))
				 (set-process-sentinel
				  proc
				  (lambda (process _event)
					(when (eq (process-status process) 'exit)
					  (let ((content (with-current-buffer output-buffer
									   (buffer-string))))
						(funcall callback content)))))))
   :async t
   :name "fetch_url"
   :description "Get the content of a url in a readable form."
   :args (list
		  '(:name "url"
				  :type string
				  :description "The url to fetch."))
   :category "web"
   :include t))

(setq gptel-tools (list
				   meow/gptel-tool-search
				   meow/gptel-tool-fetch-url))

(provide 'meow-ai)
;;; meow-ai.el ends here
