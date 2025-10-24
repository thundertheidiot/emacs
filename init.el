;; -*- lexical-binding: t; -*-
(setq package-enable-at-startup nil)
(setq package-archives nil)

(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; TODO fix
(use-package eglot-booster)
(use-package emsg-blame)
(use-package diff-hl)

(require 'meow/helpers)
(require 'meow/cleanup)
(require 'meow/undo)
(require 'meow/keybindings)
(require 'meow/lsp)
(require 'meow/programming)
(require 'meow/lang/rust)
(require 'meow/lang/c\#)
(require 'meow/lang/misc)
(require 'meow/git)
(require 'meow/ui)
(require 'meow/dump)
;;; init.el ends here
