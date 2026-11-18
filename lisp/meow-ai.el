;; -*- lexical-binding: t; -*-
(require 'plz)

(defvar meow/gptel-directory "~/Documents/gptel/")

(defun meow/gptel-screenshot ()
  "On niri, add screenshot to buffer."
  (interactive)
  (let* ((media-dir (expand-file-name "media" meow/gptel-directory))
		 (filename (expand-file-name (format-time-string "%Y-%m-%d-%H-%M-%S-%N.png") media-dir)))
    (unless (file-directory-p media-dir)
      (make-directory media-dir t))
    (when (= 0 (call-process "niri" nil nil nil "msg" "action" "screenshot" "--path" filename))
      (with-timeout
		  (30 (user-error "Timeout waiting for screenshot"))
		(while (not (file-exists-p filename))
		  (sit-for 0.05)))
      (insert (format (cond
					   ((eq major-mode 'org-mode) "[[%s]]")
					   (t "![screenshot](%s)"))
					  filename)))))

(defvar meow/openrouter-data nil
  "Cached data from the openrouter models endpoint.")

(defvar meow/openrouter-key-file (expand-file-name "openrouterkey" user-emacs-directory)
  "File holding the OpenRouter API key.")

(defvar meow/openrouter-retry-delay 60
  "Seconds before retrying a failed OpenRouter model fetch.")

(defvar meow/openrouter-default-models '(deepseek/deepseek-v4-flash-0731)
  "Models offered before the OpenRouter model list has been fetched.")

(defun meow/openrouter-key ()
  "Return the trimmed OpenRouter API key, or nil when there is none."
  (when (file-readable-p meow/openrouter-key-file)
	(let ((key (string-trim
				(with-temp-buffer
				  (insert-file-contents meow/openrouter-key-file)
				  (buffer-string)))))
	  (unless (string-empty-p key) key))))

(defun meow/openrouter-make-backend (models)
  "Register the OpenRouter gptel backend offering MODELS, and select it."
  (setq gptel-backend
		(gptel-make-openai "OpenRouter"
		  :host "openrouter.ai"
		  :endpoint "/api/v1/chat/completions"
		  :stream t
		  :key #'meow/openrouter-key
		  :models models)))

(defun meow/openrouter-setup ()
  "Set up openrouter for gptel.
The backend starts out with `meow/openrouter-default-models' so it works
right away. The full model list is fetched in the background, and a failed
fetch (e.g. offline) is retried after `meow/openrouter-retry-delay' seconds."
  (if-let* ((api-key (meow/openrouter-key)))
	  (progn
		(meow/openrouter-make-backend meow/openrouter-default-models)
		(meow/openrouter-fetch-models api-key))
	(message "OpenRouter: no key in %s, backend not set up" meow/openrouter-key-file)))

(defun meow/openrouter-fetch-models (api-key)
  "Fetch the OpenRouter model list with API-KEY and refresh the backend."
  (plz 'get "https://openrouter.ai/api/v1/models"
	:headers `(("Authorization" . ,(format "Bearer %s" api-key)))
	:as #'json-read
	:else (lambda (err)
			(message "OpenRouter: model fetch failed (%s), retrying in %ds"
					 (if (plz-error-p err)
						 (or (plz-error-message err)
							 (plz-error-curl-error err)
							 (when-let* ((resp (plz-error-response err)))
							   (plz-response-status resp)))
					   err)
					 meow/openrouter-retry-delay)
			(run-with-timer meow/openrouter-retry-delay nil
							#'meow/openrouter-fetch-models api-key))
	:then (lambda (data)
			(let* ((models (mapcar
							(lambda (entry)
							  (let* ((pricing (alist-get 'pricing entry))
									 (parameters (alist-get 'supported_parameters entry))
									 (input-modalities (alist-get 'input_modalities (alist-get 'architecture entry)))
									 (images (seq-find (lambda (e) (string= "image" e)) input-modalities )))
								`(,(intern (alist-get 'id entry))
								  :description ,(alist-get 'description entry)
								  :input-cost ,(* 1000000 (string-to-number
														   (alist-get 'prompt pricing)))
								  :output-cost ,(* 1000000 (string-to-number
															(alist-get 'completion pricing)))
								  :context-window ,(when-let* ((length (alist-get 'context_length entry)))
                                                     (/ length 1000))
								  :capabilities ,(seq-keep #'identity `(,(when (seq-find (lambda (e) (string= "tools" e)) parameters) 'tool-use)
																		,(when (seq-find (lambda (e) (string= "reasoning" e)) parameters) 'reasoning)
																		,(when (seq-find (lambda (e) (string= "structured_outputs" e)) parameters) 'json)
																		,(when images 'media)))
								  :mime-types ,(when images '("image/jpeg" "image/png" "image/gif" "image/webp"))
								  )))
							(alist-get 'data data))))
			  (setq meow/openrouter-data data)
			  ;; only reselect the backend if the user hasn't switched away
			  (if (or (null gptel-backend)
					  (equal "OpenRouter" (gptel-backend-name gptel-backend)))
				  (meow/openrouter-make-backend models)
				(let ((gptel-backend gptel-backend))
				  (meow/openrouter-make-backend models)))))))

(defun meow/gptel-quick-ask (prompt)
  "Ask PROMPT from the current model in a new gptel buffer."
  (interactive "MAsk: ")
  (let ((buffer (gptel (format-time-string "gptel-%Y%m%d-%H:%M:%S.org")
					   t
					   (format "*** %s" prompt))))
    (pop-to-buffer buffer (if (bound-and-true-p gptel-mode)
                              '(display-buffer-same-window)
                            gptel-display-buffer-action))
    (goto-char (point-max))
    (olivetti-mode -1)
    (visual-line-mode 1)
    (gptel-send)))

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
      (gptel--set-with-scope 'gptel--request-params
                             `(:reasoning_effort ,effort)
                             gptel--set-buffer-locally)
    (gptel--set-with-scope 'gptel--request-params nil gptel--set-buffer-locally)))

(advice-add 'gptel--infix-provider :after #'meow/gptel-openrouter-set-reasoning)

(use-package gptel
  :config
  (require 'gptel-autoloads)
  (require 'gptel-context)
  (meow/openrouter-setup)
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

(use-package gptel-zai
  :config
  (gptel-zai-make-backend))

(defun slop/message-zai-limit (response)
  "Show usage percentage and time-until-reset from RESPONSE."
  (let* ((data      (cdr (assq 'data response)))
         (level     (cdr (assq 'level data)))
         (limits    (cdr (assq 'limits data)))
         (msgs      '()))
    (dotimes (i (length limits))
      (let* ((limit      (aref limits i))
             (type       (cdr (assq 'type limit)))
             (percentage (cdr (assq 'percentage limit)))
             (next-reset (cdr (assq 'nextResetTime limit)))
             (remaining  (cdr (assq 'remaining limit)))
             (secs       (when (and next-reset (> next-reset 0))
                           (/ (- next-reset (* 1000.0 (float-time))) 1000.0)))
             (human      (when secs (format-seconds "%dd %Hh %Mm %Ss" secs))))
        (push (format "%s: %s%% used%s, resets in %s"
                      type percentage
                      (if remaining (format " (%s left)" remaining) "")
                      (or human "unknown"))
              msgs)))
    (message "%s [level: %s]"
             (string-join (reverse msgs) " | ")
             level)))

(defun meow/show-zai-limits ()
  "Fetch and show the current zai limits for the api key."
  (interactive)
  (plz 'get "https://api.z.ai/api/monitor/usage/quota/limit"
	:headers `(("Authorization" . ,(format "Bearer %s" (gptel-zai-api-key))))
	:as #'json-read
	:then #'slop/message-zai-limit))

;; tools

(defvar meow/gptel-tool-search
  (gptel-make-tool
   :function (lambda (callback query)
			   (let ((url (format "http://127.0.0.1:8080/search?q=%s&format=json"
								  (url-hexify-string query))))
                 (url-retrieve
                  url
                  (lambda (status)
                    (let* ((response-buffer (current-buffer))
                           (result
                            (condition-case err
                                (progn
                                  (when (plist-get status :error)
                                    (error "Request failed: %S" (plist-get status :error)))
                                  (when (and (bound-and-true-p url-http-response-status)
                                             (>= url-http-response-status 400))
                                    (error "HTTP %s" url-http-response-status))
                                  (goto-char (point-min))
                                  (unless (re-search-forward "\r?\n\r?\n" nil t)
                                    (error "Missing response headers"))
                                  (let ((json-object-type 'alist)
                                        (json-key-type 'symbol))
                                    (mapconcat
                                     (lambda (result)
                                       (format "%s - %s\n%s"
                                               (alist-get 'title result)
                                               (alist-get 'url result)
                                               (alist-get 'content result)))
                                     (alist-get 'results (json-read)) "\n\n")))
                              (error (format "Search failed: %s" (error-message-string err))))))
                      (unwind-protect
                          (funcall callback result)
                        (kill-buffer response-buffer)))))))
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
                    (when (memq (process-status process) '(exit signal))
                      (let ((content (if (buffer-live-p output-buffer)
                                         (with-current-buffer output-buffer (buffer-string))
                                       "Response buffer closed")))
                        (unwind-protect
                            (funcall callback
                                     (if (and (eq (process-status process) 'exit)
                                              (= (process-exit-status process) 0))
                                         content
                                       (format "Fetch failed: %s" content)))
                          (when (buffer-live-p output-buffer)
                            (kill-buffer output-buffer)))))))))
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

(defun meow/albedo ()
  "Start a new albedo session for `default-directory' in a ghostel buffer."
  (interactive)
  (let* ((directory (expand-file-name default-directory))
		 (buf (generate-new-buffer (format "*albedo %s*" (abbreviate-file-name directory)))))
	(ghostel-exec buf "albedo" (list "new" directory))
	(switch-to-buffer buf)))

(meow/leader
  "oo" '("albedo" . (lambda () (interactive)
						(select-window (meow/intelligent-split t))
						(meow/albedo)))
  "oO" '("albedo same window" . meow/albedo)
  "poo" '("albedo". (lambda () (interactive)
						(select-window (meow/intelligent-split t))
						(let ((default-directory (project-root (project-current))))
						  (meow/albedo))))
  "poO" '("albedo same window" . (lambda () (interactive)
								   (let ((default-directory (project-root (project-current))))
									 (meow/albedo))))
  "bo" '("switch to albedo" . (lambda () (interactive)
								  (consult-buffer
								   (list
									`(:name "Albedo buffer"
											:category buffer
											:face consult-buffer
											:history buffer-name-history
											:state ,#'consult--buffer-state
											:default t
											:items ,(lambda ()
													  (consult--buffer-query :sort 'visibility
																			 :as #'consult--buffer-pair
																			 :predicate (lambda (buf)
																						  (string-prefix-p "*albedo" (buffer-name buf)))))))))))

(provide 'meow-ai)
;;; meow-ai.el ends here
