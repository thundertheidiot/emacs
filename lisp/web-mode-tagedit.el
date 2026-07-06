;;; web-mode-tagedit --- Quickly edit tag attributes -*- lexical-binding: t; -*-
;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Thunder
;; Package-Requires: (web-mode consult)
;;; Commentary:

;; This package gives you a convenient way of quicly editing, creating and removing tag attributes in `web-mode' and derived modes using consult.

;;; Code:
(require 'web-mode)
(require 'consult)

(defun tagedit--get-attributes-for-elem (beg end)
  "Get and format attributes for the tag between BEG and END."
  (let* ((regex (rx
				 (or space ?\n)
				 (group-n 1
				   (opt (or ?: ?@))
				   (1+ (or word ?-)))
				 (opt ?=
					  (or
					   (seq (group-n 3 ?\")
							(group-n 2 (+? anychar))
							?\")
					   (seq (group-n 3 ?{)
							(group-n 2 (+? anychar))
							?})
					   (group-n 2 (1+ word))))))
		 (list '()))
	(save-excursion
	  (goto-char beg)
	  (while (re-search-forward regex end t)
		(push (cons
			   (match-string 1)
			   (list
				(cons 'name (match-string-no-properties  1))
				(cons 'value (match-string-no-properties 2))
				(cons 'value-beginning (match-beginning 2))
				(cons 'value-end (match-end 2))
				(cons 'name-end (match-end 1)) ;; for setting empty values
				(cons 'beginning (match-beginning 0)) ;; start of whole match
				(cons 'end (match-end 0)) ;; end of whole match
				(cons 'delimeter (match-string-no-properties 3))))  ;; " | { | nil
			  list)))
	list))

(defun tagedit--get-attributes ()
  (if-let* ((elt-beg (web-mode-element-beginning-position)))
	  (save-excursion
		(goto-char elt-beg)
		(if-let* ((beg (web-mode-tag-beginning-position))
				  (end (web-mode-tag-end-position)))
			(let* ((attributes (tagedit--get-attributes-for-elem beg end)))
			  (list (cons 'beg beg)
					(cons 'end end)
					(cons 'attributes attributes)))
		  (user-error "No tag found at point")))
	(user-error "No element found at point")))

(defun tagedit--interactive-pick (&optional require-match)
  "Attribute picker for interactively called tagedit commands.
Require match if REQUIRE-MATCH is set."
  (let* ((attributes (tagedit--get-attributes))
		 (attrs (cdr (assoc 'attributes attributes)))
		 (attr (consult--read attrs
							  :prompt "Attribute: "
							  :sort nil
							  :category 'web-mode-tagedit
							  :require-match require-match
							  :annotate (lambda (name)
										  (concat "   = "
												  (cdr (assoc 'value (cdr (assoc name attrs))))))
							  :lookup (lambda (selected candidates &rest _)
										(or (funcall #'consult--lookup-cdr selected candidates)
											selected)))))
	(list attr (cdr (assoc 'beg attributes)) (cdr (assoc 'end attributes)))))

;;; ###autoload
(defun tagedit-delete-attribute (attr tag-beg tag-end)
  "Delete attribute ATTR inside the tag between TAG-BEG and TAG-END.
The range is used to detect whether the tag is split across multiple lines."
  (interactive (tagedit--interactive-pick t))
  (let ((multiline (> (count-lines tag-beg tag-end) 1)))
	(delete-region (cdr (assoc 'beginning attr)) (cdr (assoc 'end attr)))
	(when multiline
	  (save-excursion
		(goto-char (cdr (assoc 'beginning attr)))
		(delete-line)))))

;;; ###autoload
(defun tagedit-set-attribute (attr tag-beg tag-end)
  "Insert an attribute ATTR inside the tag between TAG-BEG and TAG-END."
  (interactive (tagedit--interactive-pick))
  (save-excursion ;; this whole function is a point mutator
	(if (listp attr)
		;; existing attribute
		(let* ((name (cdr (assoc 'name attr)))
			   (old-value (cdr (assoc 'value attr)))
			   (value-beg (cdr (assoc 'value-beginning attr)))
			   (value-end (cdr (assoc 'value-end attr)))
			   (name-end (cdr (assoc 'name-end attr)))
			   (match-end (cdr (assoc 'end attr)))
			   (delimeter (cdr (assoc 'delimeter attr)))
			   (value (read-string (format "Value for %s (%s delim): "
										   name
										   (cond ((string= "{" delimeter) "{}")
												 ((string= "\"" delimeter) "\"\"")
												 (t "no")))
								   old-value)))
		  (if (string= value "")
			  (delete-region name-end match-end)
			(if (and value-beg value-end)
				(progn
				  (goto-char value-beg)
				  (delete-region value-beg value-end)
				  (insert value))
			  (progn
				(goto-char name-end)
				(insert (concat "=" value))))))
	  ;; new attribute
	  (let ((value (read-string (format "Value for %s (no delim): " attr))))
		(if (<= (count-lines tag-beg tag-end) 1)
			;; one line
			(progn
			  (goto-char tag-beg)
			  (if-let* ((space (ignore-errors
								 (search-forward " " tag-end))))
				  (progn
					(goto-char space)
					(if (string= value "")
						(insert (concat attr " "))
					  (insert (concat attr "=" value " "))))
				(progn
				  (goto-char tag-beg)
				  (re-search-forward (rx (or ?/ ?>)) (1+ tag-end))
				  (goto-char (1- (point)))
				  (if (string= value "")
					  (insert (concat " " attr " "))
					(insert (concat " " attr "=" value " "))))))
		  ;; multiline
		  (progn
			(goto-char (1+ (pos-eol)))
			(open-line 1)
			(indent-for-tab-command)
			(if (string= value "")
				(insert attr)
			  (insert (concat attr "=" value)))))))))

(when (featurep 'embark)
  (defvar-keymap embark-tagedit-map
	:doc "Embark keymap for web-mode tagedit."
	"d" #'tagedit-delete-attribute
	"s" #'tagedit-set-attribute
	"A" #'embark-act-all)

  (add-to-list 'embark-keymap-alist '(web-mode-tagedit . embark-tagedit-map)))

(provide 'web-mode-tagedit)
;;; web-mode-tagedit.el ends here
