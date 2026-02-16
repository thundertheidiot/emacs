;; -*- lexical-binding: t; -*-

(defvar meow/gptel-directory "~/Documents/gptel/")

(defun meow/gptel-generate-filename ()
  "Generate filename for a gptel buffer."
  (expand-file-name  (format "%s_%s%s"
			     (format-time-string "%Y-%m-%d-%H-%M-%S")
			     (buffer-name)
			     (if (eq major-mode 'org-mode)
				 ".org"
			       ".md"))
		     meow/gptel-directory))

(defun meow/gptel-save ()
  "Save gptel buffer."
  (interactive)
  (write-file (meow/gptel-generate-filename)))

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
		     (seq-contains-p (gui-get-selection 'CLIPBOARD 'TARGETS) 'image/png)
		     (seq-contains-p (gui-get-selection 'PRIMARY 'TARGETS) 'image/png)))
	  (sit-for 0.05)))
      (with-temp-buffer
	(insert (gui-get-selection 'CLIPBOARD 'image/png))
	(write-file filename))
      (insert (format (cond
		       ((eq major-mode 'org-mode) "[[%s]]")
		       (t "![screenshot](%s)"))
		      filename)))))

(use-package gptel
  :custom
  (gptel-model 'gemini-3-flash-preview)
  (gptel-default-mode #'org-mode)
  (gptel-track-media t)
  :config
  (require 'gptel-autoloads)
  (require 'gptel-context)
  (setq gptel-backend (gptel-make-gh-copilot "Copilot"))
  :general-config
  (meow/leader
    "a" '(:ignore t :wk "ai")
    "ao" '("gptel" . gptel)
    "aa" '("add context" . gptel-context-add)
    "am" '("gptel menu" . gptel-menu)
    "ar" '("remove context" . (lambda () (interactive) (gptel-context-remove)))
    "aR" '("remove all context" . gptel-context-remove-all))
  (meow/leader :keymaps 'gptel-mode-map
    "as" '("save chat" . meow/gptel-save)
    "aS" '("screenshot" . meow/gptel-screenshot))
  (:keymaps 'gptel-mode-map :states '(normal)
	    "RET" #'gptel-send))

(defun meow/is-github-repo-p (path)
  (let* ((path (expand-file-name path))
	 (default-directory (if (file-directory-p path)
				path
			      (file-name-directory path))))
    (with-temp-buffer
      (when (zerop (call-process "git" nil t nil "remote" "get-url" "origin"))
	(goto-char (point-min))
	(search-forward "github.com" nil t)))))

(defun meow/gptel-function-documentation (symbol)
  (documentation (intern symbol)))

(defun meow/gptel-variable-documentation (symbol)
  (documentation-property (intern symbol) 'variable-documentation))

(defun meow/gptel-symbol-source (symbol)
  (let* ((symbol (intern symbol))
	 (callable (or (functionp symbol) (macrop symbol)))
	 (find-fn (if callable
		      #'find-function-noselect
		    #'find-variable-noselect)))
    (condition-case nil
	(let* ((location (funcall find-fn symbol))
	       (buf (car location))
	       (point (cdr location)))
	  (with-current-buffer buf
	    (save-excursion
	      (goto-char point)
	      (buffer-substring-no-properties
	       (point)
	       (progn (end-of-defun) (point))))))
      (error
       (let ((obj (if callable
		      (symbol-function symbol)
		    (symbol-value symbol))))
	 (pp-to-string obj))))))


(setq gptel-tools (list
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
		    :description "Searches the web using SearXNG metasearch engine and returns formatted results including titles, URLs, and content excerpts."
		    :args (list
			   '(:name "query"
				   :type string
				   :description "The search query to execute against the search engine."))
		    :category "web"
		    :include t)
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
		    :include t)
		   (gptel-make-tool
		    :function (lambda (&optional path)
				(let ((dir (if path
					       (expand-file-name path)
					     default-directory)))
				  (mapcar #'file-truename
					  (seq-filter #'file-regular-p
						      (directory-files dir)))))
		    :name "list_directory"
		    :description "List the contents of a directory, or the default-directory if no path is given."
		    :args (list
			   '(:name "path"
				   :type string
				   :description "Path to list."
				   :optional t))
		    :category "files"
		    :include t)
		   (gptel-make-tool
		    :function (lambda (path)
				(with-temp-buffer
				  (insert-file-contents (expand-file-name path))
				  (buffer-string)))
		    :name "read_file"
		    :description "Read the contents of a file, requires user confirmation."
		    :args (list
			   '(:name "path"
				   :type string
				   :description "Path to read."))
		    :category "files"
		    :confirm (lambda (path) (not (meow/is-github-repo-p path)))
		    :include t)
		   (gptel-make-tool
		    :function #'meow/gptel-variable-documentation
		    :name "elisp_variable_documentation"
                    :description "Retreive documentation for an elisp variable."
		    :args (list
			   '(:name "symbol"
				   :type string
				   :description "Name of the variable."))
		    :category "emacs"
		    :include t)
		   (gptel-make-tool
		    :function #'meow/gptel-function-documentation
		    :name "elisp_function_documentation"
                    :description "Retreive documentation for an elisp function."
		    :args (list
			   '(:name "symbol"
				   :type string
				   :description "Name of the function."))
		    :category "emacs"
		    :include t)
		   (gptel-make-tool
		    :function #'meow/gptel-symbol-source
		    :name "elisp_view_source"
                    :description "Show the source of an elisp function or variable."
		    :args (list
			   '(:name "symbol"
				   :type string
				   :description "Name of the symbol."))
		    :category "emacs"
		    :include t)))

(provide 'meow-ai)
;;; meow-ai.el ends here
