(let ((undo-tree-dir (expand-file-name "undo-tree/" user-emacs-directory)))
  (use-package undo-tree
    :custom
    (undo-tree-auto-save-history t)
    (undo-tree-history-directory-alist `((,(rx (* nonl)) . ,undo-tree-dir)))
    :config
    (unless (file-directory-p undo-tree-dir)
      (make-directory undo-tree-dir))
    ;; cannot override the default undo without this
    (defun undo-tree-overridden-undo-bindings-p () nil)
    (global-undo-tree-mode)))

(provide 'meow/undo)
