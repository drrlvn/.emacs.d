;;; config-looks.el --- look configuration -*- lexical-binding: t; byte-compile-warnings: (not unresolved) -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(setq frame-title-format '("" invocation-name ": %b"))

(custom-set-faces
 `(default
    ((t
      :family ,(seq-find (lambda (font) (find-font (font-spec :name font))) '("Iosevka" "Fira Mono" "Ubuntu Mono"))
      :height ,(if (eq system-type 'darwin) 150 130)))))
      :height ,(if (eq system-type 'darwin) 150 120)))))

(use-package doom-themes
  :ensure
  :config
  (load-theme 'doom-one t)
  (set-face-attribute 'font-lock-keyword-face nil :slant 'italic)
  (set-face-attribute 'font-lock-preprocessor-face nil :slant 'italic)
  (set-face-attribute 'font-lock-type-face nil :weight 'bold)
  (set-face-attribute 'font-lock-function-name-face nil :weight 'bold)
  (set-face-attribute 'font-lock-constant-face nil :weight 'bold)
  (doom-themes-org-config))

(use-package window-numbering
  :ensure
  :config (window-numbering-mode 1))

(use-package powerline
  :defer
  :config (remove-hook 'focus-out-hook 'powerline-unset-selected-window))

(use-package spaceline-config
  :ensure spaceline
  :config
  (setq spaceline-responsive nil
        spaceline-workspace-numbers-unicode t
        spaceline-window-numbers-unicode t)
  (set-face-background 'spaceline-highlight-face "#51afef")
  (spaceline-toggle-minor-modes-off)
  (spaceline-spacemacs-theme))

(use-package mode-icons
  :if window-system
  :ensure
  :config (mode-icons-mode 1))

(when window-system
  (scroll-bar-mode -1)
  (horizontal-scroll-bar-mode -1))

(tool-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode 1)

(provide 'config-looks)

;;; config-looks.el ends here
