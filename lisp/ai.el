;; -*- lexical-binding: t; -*-
(use-package gptel
  :custom
  (gptel-model 'gpt-5-mini)
  :config
  (require 'gptel-autoloads)
  (setq gptel-backend (gptel-make-gh-copilot "Copilot"))
  (add-to-list 'gptel-tools
	       (gptel-make-tool
		:function (lambda (query)
			    (with-temp-message (format "Searching for: `%s`" query)
			      ;; provided by nixos
			      (let ((url (format "http://127.0.0.1:8080/search?q=%s&format=json"
						 (url-hexify-string query))))
				(with-temp-buffer
				  (url-insert-file-contents url)
				  (let ((json-response (json-read)))
				    (mapconcat (lambda (result)
						 (format "%s - %s\n%s" (cdr (assoc 'title result)) (cdr (assoc 'url result)) (cdr (assoc 'content result))))
					       (cdr (assoc 'results json-response))
					       "\n\n"))))))
		:name "search_web"
		:description "Searches the web using SearXNG metasearch engine and returns formatted results including titles, URLs, and content excerpts."
		:args (list
		       '(:name "query"
			       :type string
			       :description "The search query to execute against the search engine."))
		:category "web"
		:include t)))

(provide 'meow/ai)
