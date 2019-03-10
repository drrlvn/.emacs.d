;;; config-looks.el --- look configuration -*- lexical-binding: t; byte-compile-warnings: (not unresolved) -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(setq frame-title-format '("" invocation-name ": %b"))

(use-package cus-edit
  :config
  (when (eq (custom-face-state 'default) 'standard)
        (custom-set-faces
         `(default
            ((t
              :family ,(seq-find (lambda (font) (find-font (font-spec :name font)))
                                 '("Iosevka SS05" "Iosevka SS09" "Iosevka SS01" "Iosevka" "Ubuntu Mono"))
              :height ,(if (eq system-type 'darwin) 150 120)))))))

(eval-when-compile
  (defvar my/theme))

(use-package doom-themes
  :ensure
  :config
  (load-theme my/theme t)
  (set-face-attribute 'font-lock-keyword-face nil :slant 'italic)
  (set-face-attribute 'font-lock-preprocessor-face nil :slant 'italic)
  (set-face-attribute 'font-lock-type-face nil :weight 'bold)
  (set-face-attribute 'font-lock-function-name-face nil :weight 'bold)
  (set-face-attribute 'font-lock-constant-face nil :weight 'bold)
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure
  :config
  (setq doom-modeline-python-executable nil)
  (doom-modeline-mode 1))

(when window-system
  (scroll-bar-mode -1)
  (horizontal-scroll-bar-mode -1))

(tool-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode 1)

(provide 'config-looks)

;;; config-looks.el ends here
