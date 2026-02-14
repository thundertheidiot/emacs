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

;; libraries that are useful for elisp programming
(use-package dash)
(use-package plz)

(require 'meow-helpers)
(require 'meow-cleanup)
(require 'meow-undo)
(require 'meow-window-configuration)
(require 'meow-keybindings)
(require 'meow-lsp)
(require 'meow-misc)
(require 'meow-programming)
(require 'lang/meow-rust)
(require 'lang/meow-c\#)
(require 'lang/meow-nix)
(require 'lang/meow-misc)
(require 'meow-git)
;; (require 'meow-mode-line)
(require 'meow-terminal)
(require 'meow-ui)
(require 'meow-ai)
(require 'meow-org)
(require 'meow-media)
(require 'meow-mommy)
;;; init.el ends here
