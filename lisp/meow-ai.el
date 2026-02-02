;; -*- lexical-binding: t; -*-

(defvar meow/gptel-directory "~/Documents/gptel/")

(defun meow/gptel-generate-filename ()
  "Generate filename for a gptel buffer."
  (expand-file-name  (format "%s_%s.md"
			     (format-time-string "%Y-%m-%d-%H-%M-%S")
			     (buffer-name))
		     meow/gptel-directory))

(defun meow/gptel-save ()
  "Save gptel buffer."
  (interactive)
  (write-file (meow/generate-gptel-filename)))

(defun meow/gptel-screenshot ()
  "On niri, add screenshot to buffer."
  (interactive)
  (let* ((media-dir (expand-file-name "media" meow/gptel-directory))
	 (filename (expand-file-name (format-time-string "%Y-%m-%d-%H-%M-%S.png") media-dir)))
    (unless (file-directory-p media-dir)
      (make-directory media-dir t))
    (when (= 0 (shell-command "niri msg action screenshot"))
      (with-timeout
	  (30 (error "Timeout waiting for clipboard"))
	(while (not (or
		     (seq-contains (gui-get-selection 'CLIPBOARD 'TARGETS) 'image/png)
		     (seq-contains (gui-get-selection 'PRIMARY 'TARGETS) 'image/png)))
	  (sit-for 0.05)))
      (with-temp-buffer
	(insert (gui-get-selection 'CLIPBOARD 'image/png))
	(write-file filename))
      (insert (format "![screenshot](%s)" filename)))))

(use-package gptel
  :custom
  (gptel-model 'gpt-5-mini)
  :config
  (require 'gptel-autoloads)
  (require 'gptel-context)
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
		:include t))
  :general-config
  (meow/leader
    "a" '(:ignore t :wk "ai")
    "ao" '("gptel" . gptel)
    "aa" '("add context" . gptel-context-add)
    "am" '("gptel menu" . gptel-menu)
    "as" '("save chat" . meow/gptel-save)
    "aS" '("screenshot" . meow/gptel-screenshot)
    "ar" '("remove context" . (lambda () (interactive) (gptel-context-remove)))
    "aR" '("remove all context" . gptel-context-remove-all))
  (:keymaps 'gptel-mode-map :states '(normal)
	    "RET" #'gptel-send))

(provide 'meow-ai)
;;; meow-ai.el ends here
