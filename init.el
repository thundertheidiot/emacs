;; -*- lexical-binding: t; -*-
;; ignore garbage collection for snappier init
(setq gc-cons-threshold most-positive-fixnum)

(setq package-enable-at-startup nil)
(setq package-archives nil)

(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

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
(require 'lang/meow-godot)
(require 'lang/meow-misc)
(require 'meow-mode-line)
(require 'meow-terminal)
(require 'meow-ui)
(require 'meow-git)
(require 'meow-ai)
(require 'meow-org)
(require 'meow-mpd)
(require 'meow-media)
(require 'meow-mommy)

(require 'meow-theme)

;; gc setup
(unless (featurep 'igc)
  (setq gc-cons-threshold (* 1024 1024 64))
  (add-hook 'minibuffer-setup-hook (lambda ()
				     (setq gc-cons-threshold most-positive-fixnum)))
  (add-hook 'minibuffer-exit-hook (lambda ()
				    (setq gc-cons-threshold (* 1024 1024 64)))))

(run-with-idle-timer 10 t #'garbage-collect)

;;; init.el ends here
