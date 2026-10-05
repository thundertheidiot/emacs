;;; claudepet-integrations.el --- Optional ClaudePet integrations -*- lexical-binding: t; -*-

;;; Commentary:
;; Enable `claudepet-integrations-mode' for compilation completion notifications.
;; Clicking the notification visits its compilation buffer.  Loading this library
;; alone does not load or enable the pet.

;;; Code:

(declare-function claudepet-source-request "claudepet" (source name &optional callback))
(defvar claudepet-mode)
(defvar claudepet-start-hook)
(defvar claudepet-stop-hook)
(defvar compilation-finish-functions)
(defvar claudepet-integrations-mode nil)

(when (fboundp 'claudepet--integrations-stop)
  (if (bound-and-true-p claudepet-integrations-mode)
      (claudepet-integrations-mode -1)
    (claudepet--integrations-stop)))

(defgroup claudepet-integrations nil
  "Optional integrations for the Claude companion."
  :group 'applications)

(defvar claudepet--integrations-compilation-hook-installed nil
  "Whether this module owns its compilation hook registration.")

(defun claudepet--integrations-compilation-finish (buffer result &rest _)
  "Notify compilation RESULT, with a click action visiting live BUFFER."
  (when (and claudepet-integrations-mode claudepet-mode)
    (claudepet-source-request
     'compilation
     (if (and (stringp result) (string-match-p "\\`finished\\(?:[ \n]\\|\\'\\)" result))
         'done 'error)
     (lambda ()
       (when (buffer-live-p buffer)
         (switch-to-buffer buffer))))))

(defun claudepet--integrations-start ()
  "Install integration hooks while the global pet mode is running."
  (unless (memq #'claudepet--integrations-compilation-finish
                (and (boundp 'compilation-finish-functions)
                     (default-value 'compilation-finish-functions)))
    (add-hook 'compilation-finish-functions #'claudepet--integrations-compilation-finish)
    (setq claudepet--integrations-compilation-hook-installed t)))

(defun claudepet--integrations-stop ()
  "Remove only integration hook registrations owned by this module."
  (when claudepet--integrations-compilation-hook-installed
    (remove-hook 'compilation-finish-functions #'claudepet--integrations-compilation-finish)
    (setq claudepet--integrations-compilation-hook-installed nil)))

(define-minor-mode claudepet-integrations-mode
  "Attach optional compilation notifications to ClaudePet's lifecycle hooks."
  :global t :group 'claudepet-integrations
  (if claudepet-integrations-mode
      (condition-case err
          (progn
            (require 'claudepet)
            (add-hook 'claudepet-start-hook #'claudepet--integrations-start)
            (add-hook 'claudepet-stop-hook #'claudepet--integrations-stop)
            (when claudepet-mode (claudepet--integrations-start)))
        (error
         (claudepet-integrations-mode -1)
         (signal (car err) (cdr err))))
    (unwind-protect (claudepet--integrations-stop)
      (remove-hook 'claudepet-start-hook #'claudepet--integrations-start)
      (remove-hook 'claudepet-stop-hook #'claudepet--integrations-stop))))

(defun claudepet-integrations-unload-function ()
  "Detach integrations before unloading this library."
  (claudepet-integrations-mode -1)
  nil)

(provide 'claudepet-integrations)
;;; claudepet-integrations.el ends here
