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

(defun meow/gptel-clipboard ()
  "Add clipboard content to the context."
  (interactive)
  (let ((clipboard (gui-get-selection 'CLIPBOARD 'TARGETS))
	(primary (gui-get-selection 'PRIMARY 'TARGETS)))
    (cond
     ((or
       (seq-contains-p clipboard 'image/png)
       (seq-contains-p primary 'image/png))
      (let* ((media-dir (expand-file-name "media" meow/gptel-directory))
	     (filename (expand-file-name (format-time-string "%Y-%m-%d-%H-%M-%S.png") media-dir)))
	(unless gptel-mode
	  (user-error "Images only work in a gptel buffer"))
	(with-temp-buffer
	  (insert (gui-get-selection 'CLIPBOARD 'image/png))
	  (write-file filename))
	(insert (format (cond
			 ((eq major-mode 'org-mode) "[[%s]]")
			 (t "![screenshot](%s)"))
			filename))))
     (t (with-current-buffer (generate-new-buffer "*gptel-clipboard-context*")
	  (insert (gui-get-selection 'CLIPBOARD 'text/plain))
	  (gptel-add))))))

(defun meow/gptel-quick-ask (prompt)
  "Ask PROMPT from gptel."
  (interactive "MAsk: ")
  (select-window (meow/intelligent-split t))
  (switch-to-buffer
   (gptel (format-time-string "gptel-%Y%m%d-%H:%M:%S.org")
	  t
	  (format "*** %s" prompt)))
  (goto-char (point-max))
  (gptel-send))

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
    "ac" '("quick ask" . meow/gptel-quick-ask)
    "aa" '("add context" . gptel-context-add)
    "am" '("gptel menu" . gptel-menu)
    "ap" '("insert from clipboard" . meow/gptel-clipboard)
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
   :description "Searches the web using SearXNG metasearch engine and returns formatted results including titles, URLs, and content excerpts."
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

(defun meow/gptel-async (prompt model callback &optional backend system-message tools)
  "Call gptel asynchronously, with PROMPT using MODEL.
Upon completion CALLBACK is called."
  (let ((gptel-model model)
	(gptel-backend (or backend gptel-backend))
	(gptel-use-tools t)
	(gptel-use-context t)
	(gptel-tools (or tools gptel-tools))
	(gptel-include-tool-results nil)) ;; won't be shown anyway, can mess with stupider llms
    (gptel-request
	prompt
      :callback (lambda (response info)
		  (funcall callback response))
      :system (or system-message "Respond concisely, do not ask questions or provide suggestions."))))

(defvar meow/gptel-tool-subagent-raw
  (gptel-make-tool
   :function (lambda (callback prompt &optional model)
	       (meow/gptel-async prompt model callback))
   :async t
   :args (list
	  '(:name "prompt"
		  :type string
		  :description "Prompt for the sub agent.")
	  '(:name "model"
		  :type string
		  :optional t
		  :description "Model to use, prefer gpt-5-mini if possible for economical reasons."))
   :name "raw_subagent"
   :description "Use a subagent to execute a task."
   :category "advanced"
   :include t))

(defvar meow/gptel-tool-subagent
  (gptel-make-tool
   :function (lambda (callback prompt)
	       (meow/gptel-async prompt 'gpt-5-mini
				 (lambda (response)
				   (meow/gptel-async
				    (format "Summarize the following text:\n%s" response)
				    'gpt-5-mini
				    callback))))
   :async t
   :args (list
	  '(:name "prompt"
		  :type string
		  :description "Prompt for the subagent."))
   :name "subagent"
   :description "Use a subagent to execute a task, the agent has the same tools available as you do, though it does not have the same intelligence."
   :category "advanced"
   :include t))

(defvar meow/gptel-tool-list-directory
  (gptel-make-tool
   :function (lambda (&optional path)
	       (let* ((dir (if path
			       (expand-file-name path)
			     default-directory))
		      (default-directory dir))
		 (mapconcat #'identity
			    (mapcar (lambda (p) (expand-file-name p dir))
				    (seq-filter (lambda (p) (not (or (string= "." p)
								     (string= ".." p))))
						(directory-files dir)))
			    "\n")))
   :name "list_directory"
   :description "List the contents of a directory, or the default-directory if no path is given."
   :args (list
	  '(:name "path"
		  :type string
		  :description "Path to list."
		  :optional t))
   :category "files"
   :include t))

(defvar meow/gptel-tool-make-tool
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
   :include t))

(defvar meow/gptel-tool-git-clone
  (gptel-make-tool
   :function (lambda (callback repo)
	       (let ((tmp-dir (make-temp-file "gptel-git" t))
		     (default-directory temporary-file-directory))
		 (magit-run-git-async "clone" repo (magit-convert-filename-for-git tmp-dir))
		 (set-process-sentinel
		  magit-this-process
		  (lambda (proc event)
		    (when (eq (process-status proc) 'exit)
		      (funcall callback tmp-dir))))))
   :async t
   :name "git_clone"
   :description "Clone a git repository to a temporary path, returns the path to the repository."
   :args (list
	  '(:name "repo"
		  :type string
		  :description "Repository url."))
   :category "git"
   :confirm t
   :include t))

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


(defvar meow/gptel-tool-emacs-variable-documentation
  (gptel-make-tool
   :function #'meow/gptel-variable-documentation
   :name "elisp_variable_documentation"
   :description "Retreive documentation for an elisp variable."
   :args (list
	  '(:name "symbol"
		  :type string
		  :description "Name of the variable."))
   :category "emacs"
   :include t))

(defvar meow/gptel-tool-emacs-function-documentation
  (gptel-make-tool
   :function #'meow/gptel-function-documentation
   :name "elisp_function_documentation"
   :description "Retreive documentation for an elisp function."
   :args (list
	  '(:name "symbol"
		  :type string
		  :description "Name of the function."))
   :category "emacs"
   :include t))

(defvar meow/gptel-tool-emacs-symbol-source
  (gptel-make-tool
   :function #'meow/gptel-symbol-source
   :name "elisp_view_source"
   :description "Show the source of an elisp function or variable."
   :args (list
	  '(:name "symbol"
		  :type string
		  :description "Name of the symbol."))
   :category "emacs"
   :include t))

(setq gptel-tools (list
		   meow/gptel-tool-search
		   meow/gptel-tool-fetch-url))

(provide 'meow-ai)
;;; meow-ai.el ends here
