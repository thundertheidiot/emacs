;; -*- lexical-binding: t; -*-
(setq package-enable-at-startup nil)
(setq package-archives nil)

;; TODO fix
(use-package eglot-booster)
(use-package emsg-blame)
(use-package diff-hl)

(require 'meow/helpers)
(require 'meow/cleanup)
(require 'meow/undo)
(require 'meow/keybindings)
(require 'meow/lsp)
(require 'meow/ui)
(require 'meow/dump)
;;; init.el ends here
