;;; -*- lexical-binding: t; -*-
(use-package gdscript-mode
  :mode "\\.gdscript\\'"
  :mode "\\.gd\\'"
  :hook (gdscript-mode . eglot-ensure))

;; the default regex given to directory-files-recursively is ".*.tscn", which adds some cfg files and other useless stuff into the mix
;; this override sets a more sensible regex
(advice-add 'gdscript-project--select-scene :override
	    (lambda ()
	      (let* ((rl (gdscript-util--find-project-configuration-file))
		     (scene-list (mapcar (lambda (x) (file-relative-name x rl)) (directory-files-recursively rl "\\.tscn$" t)))
		     (prompt (format "Select scene to run (%s): " (buffer-name)))
		     (selected-scene (gdscript-util--read scene-list prompt)))
		selected-scene)))

(use-package gdshader-mode
  :mode "\\.gdshader\\'"
  :config
  (add-to-list 'cape-keyword-list
	       (append '(gdshader-mode) gdshader-all-keywords))
  :hook
  (gdshader-mode . (lambda ()
		     (setq-local completion-at-point-functions
				 (list (cape-capf-super
					#'cape-keyword
					#'cape-dabbrev)
				       t)))))

(provide 'lang/meow-godot)
;;; meow-misc.el ends here
