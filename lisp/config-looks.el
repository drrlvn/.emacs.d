;;; config-looks.el --- look configuration -*- lexical-binding: t; byte-compile-warnings: (not unresolved) -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq frame-title-format '("" invocation-name ": %b"))

(eval-when-compile
  (defvar my/theme)
  (defvar my/font-family)
  (defvar my/font-height))

(set-face-attribute 'default nil
                    :family my/font-family
                    :height my/font-height)

(use-package doom-themes
  :straight t
  :config
  (load-theme my/theme t)
  (set-face-attribute 'font-lock-keyword-face nil :slant 'italic)
  (set-face-attribute 'font-lock-preprocessor-face nil :slant 'italic)
  (set-face-attribute 'font-lock-type-face nil :weight 'bold)
  (set-face-attribute 'font-lock-function-name-face nil :weight 'bold)
  (set-face-attribute 'font-lock-constant-face nil :weight 'bold)
  (doom-themes-org-config))

(use-package doom-modeline
  :straight t
  :config (doom-modeline-mode 1))

(when window-system
  (scroll-bar-mode -1)
  (horizontal-scroll-bar-mode -1))

(tool-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode 1)

(provide 'config-looks)

;;; config-looks.el ends here
