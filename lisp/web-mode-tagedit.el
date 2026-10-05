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
  "Get attributes for the tag between BEG and exclusive END.
Reject incomplete syntax rather than return partial edit targets."
  (let ((attributes nil)
        (parse-sexp-ignore-comments t))
    (save-excursion
      (goto-char beg)
      (unless (and (looking-at (rx "<" (+ (not (any space "/>")))))
                   (<= (match-end 0) end))
        (user-error "No opening tag found"))
      (goto-char (match-end 0))
      (while (progn
               (skip-chars-forward " \t\r\n" end)
               (and (< (point) end) (not (looking-at "/?>"))))
        (let ((name-beg (point)) name-end attr-end value-beg value-end delimiter)
          (unless (looking-at (rx (+ (not (any space "=/><\"'{}")))))
            (user-error "Unsupported attribute syntax"))
          (setq name-end (match-end 0))
          (unless (<= name-end end)
            (user-error "Unterminated attribute name"))
          (goto-char name-end)
          (skip-chars-forward " \t\r\n" end)
          (if (eq (char-after) ?=)
              (progn
                (forward-char)
                (skip-chars-forward " \t\r\n" end)
                (pcase (char-after)
                  ((or ?\" ?\')
                   (setq delimiter (char-to-string (char-after)))
                   (forward-char)
                   (setq value-beg (point))
                   (unless (search-forward delimiter end t)
                     (user-error "Unterminated quoted attribute"))
                   (setq value-end (1- (point))))
                  (?{
                   (setq delimiter "{" value-beg (1+ (point)))
                   ;; Scan balanced JSX expressions, including nested objects and strings.
                   (with-syntax-table (make-syntax-table)
                     (modify-syntax-entry ?{ "(}")
                     (modify-syntax-entry ?} "){")
                     (modify-syntax-entry ?\' "\"")
                     (modify-syntax-entry ?` "\"")
                     (modify-syntax-entry ?/ ". 124b")
                     (modify-syntax-entry ?* ". 23")
                     (modify-syntax-entry ?\n "> b")
                     (let ((close (condition-case nil
                                      (save-restriction
                                        (narrow-to-region (point) end)
                                        (scan-sexps (point) 1))
                                    (scan-error nil))))
                       (unless (and close (<= close end))
                         (user-error "Unterminated attribute expression"))
                       ;; The sexp scanner cannot distinguish JS regexps from division.
                       (save-excursion
                         (while (re-search-forward "[/`]" close t)
                           (let* ((pos (match-beginning 0))
                                  (state (save-excursion
                                           (parse-partial-sexp (1- value-beg) pos))))
                             (cond
                              ((or (nth 3 state) (nth 4 state)))
                              ((and (eq (char-after pos) ?/)
                                    (memq (char-after (1+ pos)) '(?/ ?*)))
                               (forward-char))
                              (t (user-error "Unsupported regexp, slash operator or template literal in attribute"))))))
                       (goto-char close)))
                   (setq value-end (1- (point))))
                  (_
                   (setq value-beg (point))
                   (skip-chars-forward "^ \t\r\n<>\"'=`" end)
                   (setq value-end (point))
                   (when (= value-beg value-end)
                     (user-error "Missing attribute value"))))
                (unless (or (>= (point) end)
                            (looking-at (rx (or space "/>" ">"))))
                  (user-error "Unsupported attribute value syntax"))
                (setq attr-end (point)))
            (setq attr-end name-end))
          (push (cons (buffer-substring-no-properties name-beg name-end)
                      (list
                       (cons 'name (buffer-substring-no-properties name-beg name-end))
                       (cons 'value (when value-beg
                                      (buffer-substring-no-properties value-beg value-end)))
                       (cons 'value-beginning value-beg)
                       (cons 'value-end value-end)
                       (cons 'name-end name-end)
                       (cons 'beginning (save-excursion
                                          (goto-char name-beg)
                                          (skip-chars-backward " \t" beg)
                                          (point)))
                       (cons 'end attr-end)
                       (cons 'delimeter delimiter)))
                attributes)))
      (unless (and (looking-at "/?>") (= (match-end 0) end))
        (user-error "Unterminated opening tag")))
    attributes))

(defun tagedit--get-attributes ()
  (if-let* ((elt-beg (web-mode-element-beginning-position)))
	  (save-excursion
		(goto-char elt-beg)
		(if-let* ((beg (web-mode-tag-beginning-position))
				  (end (web-mode-tag-end-position)))
			(let* ((attributes (tagedit--get-attributes-for-elem beg (1+ end))))
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
        (beginning-of-line)
        (when (looking-at (rx (* (any " \t")) line-end))
          (delete-region (point) (min (point-max) (1+ (line-end-position)))))))))

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
               (value (read-string
                       (format "Value for %s (%s delim): " name
                               (cond ((string= "{" delimeter) "{}")
                                     ((string= "\"" delimeter) "\"\"")
                                     ((string= "'" delimeter) "''")
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
      (let ((value (read-string (format "Value for %s (no delim): " attr)))
            (multiline (> (count-lines tag-beg tag-end) 1)))
        (goto-char tag-beg)
        (re-search-forward (rx "<" (+ (not (any space "/>")))) tag-end)
        (if multiline
            (progn
              (insert "\n")
              (open-line 1)
              (indent-for-tab-command))
          (insert " "))
        (insert (if (string-empty-p value) attr (concat attr "=" value)))))))

(when (featurep 'embark)
  (defvar-keymap embark-tagedit-map
	:doc "Embark keymap for web-mode tagedit."
	"d" #'tagedit-delete-attribute
	"s" #'tagedit-set-attribute
	"A" #'embark-act-all)

  (add-to-list 'embark-keymap-alist '(web-mode-tagedit . embark-tagedit-map)))

(provide 'web-mode-tagedit)
;;; web-mode-tagedit.el ends here
