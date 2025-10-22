(use-package general
  :demand t
  :config
  (general-evil-setup))

(general-create-definer meow/leader
  :states '(normal insert visual emacs motion)
  :keymaps 'override
  :prefix "SPC"
  :global-prefix "C-SPC")

(general-create-definer meow/local
  :states '(normal insert visual emacs motion)
  :keymaps 'override
  :prefix "SPC l"
  :global-prefix "C-SPC l")

(provide 'meow/general)
