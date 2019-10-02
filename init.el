;;; init.el --- emacs config -*- lexical-binding: t; byte-compile-warnings: (not unresolved) -*-
;;; Commentary:
;;; Code:

(let ((gc-cons-threshold-original gc-cons-threshold)
      (file-name-handler-alist-original file-name-handler-alist))
  (run-with-idle-timer 0 nil (lambda () (setq inhibit-message nil
                                              file-name-handler-alist file-name-handler-alist-original
                                              gc-cons-threshold gc-cons-threshold-original))))

(defconst my/cargo-check-flags "--all-features --tests --examples")
(defconst my/cargo-build-flags "--all-features")
(defconst my/site-config-directory "~/.emacs.site.d/")

(setq gc-cons-threshold (* 100 1024 1024)
      file-name-handler-alist nil
      inhibit-message t
      load-prefer-newer t
      custom-file (concat user-emacs-directory "custom.el")
      package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(push (concat user-emacs-directory "lisp") load-path)
(defvar straight-recipes-emacsmirror-use-mirror nil)
(require 'straight-bootstrap)
(require 'straight)
(setq straight-profiles
      `((nil . ,(concat user-emacs-directory "lockfile.el"))
        (site . ,(concat my/site-config-directory "lockfile.el"))))

(straight-use-package 'use-package)

(eval-when-compile
  (defvar use-package-enable-imenu-support)
  (setq use-package-enable-imenu-support t)
  (require 'use-package)
  (require 'bind-key))

(require 'config-custom)
(require 'config-defuns-autoloads)

(let ((straight-current-profile 'site))
  (my/load-if-exists (concat my/site-config-directory "init.el")))

(bind-key "<escape>" #'keyboard-escape-quit)
(bind-key "C-x r q" #'save-buffers-kill-emacs)
(unbind-key "C-x C-c")
(bind-key "<f5>" #'my/revert-buffer-no-confirmation)
(bind-key "<f11>" #'toggle-frame-fullscreen)
(bind-key "S-<f11>" #'whitespace-cleanup)
(bind-key "C-<f12>" #'my/magit-status-config-project)

(bind-key "C-<delete>" #'kill-word)
(bind-key "M-SPC" #'cycle-spacing)

(bind-key "C-<tab>" #'previous-buffer)
(bind-key "<mouse-8>" #'previous-buffer)
(bind-key "C-S-<iso-lefttab>" #'next-buffer)
(bind-key "C-S-<tab>" #'next-buffer)
(bind-key "<mouse-9>" #'next-buffer)

(bind-key "C-`" #'pop-to-mark-command)
(bind-key "C-M-`" #'pop-global-mark)

(bind-key "<C-M-home>" #'first-error)
(bind-key "<C-M-prior>" #'previous-error)
(bind-key "<C-M-next>" #'next-error)

(bind-key "C-z" #'repeat)
(unbind-key "C-x C-z")
(bind-key "C-!" #'kill-this-buffer)
(bind-key "C-M-!" #'my/kill-buffer-other-window)

(bind-key "C-c C-<return>" #'delete-blank-lines)
(bind-key "C-c d" #'my/diff-current-buffer-with-file)
(bind-key "C-c C-;" #'my/toggle-comment-line-or-region)
(bind-key "M-s M-s" #'sort-lines)

(bind-key "C-+" #'my/increment-number-at-point)
(bind-key "C-M-+" #'my/decrement-number-at-point)

(bind-key "C-x n r" #'narrow-to-region)
(bind-key "C-x n n" #'my/narrow-or-widen-dwim)

(bind-key [remap goto-line] #'my/goto-line-with-feedback)

(bind-key "M-p" #'my/scroll-down)
(bind-key "M-n" #'my/scroll-up)

(bind-key "M-z" #'my/zap-up-to-char)
(bind-key "M-Z" #'zap-to-char)

(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)

(fset #'yes-or-no-p #'y-or-n-p)

(remove-hook 'kill-buffer-query-functions #'process-kill-buffer-query-function)

(defun display-startup-echo-area-message () ".")

(setq auto-save-default nil
      auto-window-vscroll nil
      backup-directory-alist `((".*" . ,temporary-file-directory))
      comment-padding nil
      confirm-kill-processes nil
      create-lockfiles nil
      diff-switches "-u"
      disabled-command-function nil
      history-length 500
      indicate-buffer-boundaries 'left
      indicate-empty-lines t
      inhibit-startup-screen t
      initial-scratch-message nil
      kill-whole-line t
      large-file-warning-threshold (* 100 1024 1024)
      mouse-wheel-progressive-speed nil
      resize-mini-windows t
      ring-bell-function 'ignore
      scroll-conservatively 10000
      scroll-margin 5
      scroll-preserve-screen-position t
      visual-order-cursor-movement t
      )

(if (eq system-type 'windows-nt)
    (progn
      (eval-when-compile (defvar w32-lwindow-modifier))
      (setq w32-lwindow-modifier 'super)
      (w32-register-hot-key [s-s]))
  (setq shell-file-name "/bin/sh"))

(setq-default comment-column 0
              fill-column 100
              fringes-outside-margins t
              indent-tabs-mode nil
              tab-width 4
              cursor-type 'bar
              bidi-paragraph-separate-re "^"
              bidi-paragraph-start-re "^"
              )

(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)

(require 'config-looks)

(use-package dash
  :straight t
  :defer)

(use-package f
  :straight t
  :defer)

(use-package vc-hooks
  :defer
  :config (setq vc-follow-symlinks t))

(use-package exec-path-from-shell
  :straight t
  :if (eq system-type 'darwin)
  :config (exec-path-from-shell-initialize))

(use-package nxml-mode
  :defer
  :config (setq nxml-slash-auto-complete-flag t))

(use-package browse-url
  :bind ("<C-M-return>" . browse-url-at-point))

(use-package mwim
  :straight t
  :bind (("<home>" . mwim-beginning)
         ("<end>" . mwim-end)))

(use-package misc
  :bind ("C-$" . copy-from-above-command))

(eval
 `(use-package windmove
    :bind ((,(concat my/windmove-modifier "-<left>") . windmove-left)
           (,(concat my/windmove-modifier "-<right>") . windmove-right)
           (,(concat my/windmove-modifier "-<up>") . windmove-up)
           (,(concat my/windmove-modifier "-<down>") . windmove-down))))

(use-package hydra
  :straight t
  :bind ("<f8>" . my/hydra-error/body)
  :config (defhydra my/hydra-error ()
            ("P" first-error "first")
            ("n" next-error "next")
            ("p" previous-error "prev")
            ("v" recenter-top-bottom "recenter")
            ("q" nil "quit")))

(use-package hl-line
  :config (global-hl-line-mode 1))

(use-package saveplace
  :config (save-place-mode 1))

(use-package calendar
  :defer
  :config (setq calendar-weekend-days '(5 6)))

(use-package compile
  :defer
  :hook (compilation-filter . my/colorize-compilation-buffer)
  :config (setq compilation-scroll-output 'first-error
                compilation-read-command nil
                compilation-ask-about-save nil))

(use-package ediff
  :defer
  :config (setq ediff-split-window-function 'split-window-horizontally))

(use-package doc-view
  :defer
  :config (setq doc-view-continuous t
                doc-view-resolution 300))

(use-package dired
  :config (setq dired-recursive-deletes 'always
                dired-listing-switches (format "%s -AlFh" (if (eq system-type 'gnu/linux) "-X --group-directories-first" ""))))

(use-package dired-aux
  :defer
  :config (setq dired-isearch-filenames t))

(use-package dired-x
  :hook (dired-mode . dired-omit-mode))

(use-package org
  :bind (("<f10>" . org-agenda)
         ("C-<f10>" . org-capture)
         ("M-<f10>" . org-capture-goto-last-stored)
         :map org-mode-map
         ("C-s-<up>" . org-move-subtree-up)
         ("C-s-<down>" . org-move-subtree-down)
         ("M-p" . org-previous-link)
         ("M-n" . org-next-link))
  :hook (org-mode . my/org-mode-hook)
  :config
  (setq org-replace-disputed-keys t
        org-src-fontify-natively t
        org-startup-indented t))

(use-package org-bullets
  :straight t
  :hook (org . org-bullets-mode))

(use-package ox-html
  :defer
  :config (setq org-html-postamble nil))

(use-package glasses
  :defer
  :config (setq glasses-separate-parentheses-p nil
                glasses-uncapitalize-p t))

(use-package imenu
  :defer
  :config (setq imenu-auto-rescan t))

(use-package tramp
  :defer
  :config (setq tramp-default-method "scpx"))

(use-package uniquify
  :defer
  :config (setq uniquify-buffer-name-style 'post-forward
                uniquify-separator ":"))

(use-package vlf-setup
  :straight vlf)

(use-package server
  :if window-system
  :config
  (unless (server-running-p)
    (server-start)))

(use-package autorevert
  :config
  (setq auto-revert-verbose nil
        global-auto-revert-non-file-buffers t)
  (global-auto-revert-mode 1))

(use-package beginend
  :straight t
  :config (beginend-global-mode 1))

(use-package bln-mode
  :straight t
  :bind (("M-[" . bln-backward-half)
         ("M-]" . bln-forward-half)))

(use-package crux
  :straight t
  :bind (("M-<return>" . crux-smart-open-line)
         ("M-S-<return>" . crux-smart-open-line-above)
         ("<f12>" . crux-cleanup-buffer-or-region)
         ("S-<f12>" . crux-find-user-init-file)
         ("C-S-<backspace>" . crux-kill-whole-line)
         ("C-c n" . crux-cleanup-buffer-or-region)
         ("C-c C-e" . crux-eval-and-replace)
         ("C-c C-r" . crux-transpose-windows)
         ("C-^" . crux-top-join-line)))

(use-package recentf
  :config
  (setq recentf-max-saved-items 1000)
  (recentf-mode 1))

(use-package prescient
  :straight t
  :config (prescient-persist-mode 1))

(use-package wgrep
  :straight t
  :defer
  :config (setq wgrep-auto-save-buffer t))

(use-package ivy
  :straight t
  :demand
  :bind (("C-S-s". ivy-resume)
         ("H-b" . counsel-switch-buffer)
         :map ivy-minibuffer-map
         ("<return>" . ivy-alt-done)
         ("C-j" . ivy-done)
         ("<C-down>" . ivy-next-line-and-call)
         ("<C-up>" . ivy-previous-line-and-call))
  :config
  (setq ivy-use-virtual-buffers t
        ivy-use-selectable-prompt t
        ivy-virtual-abbreviate 'full
        ivy-count-format "(%d/%d) "
        ivy-extra-directories '("./")
        ivy-more-chars-alist '((t . 2))
        ivy-initial-inputs-alist nil)
  (push '(emacs-lisp-mode . swiper-match-face-1) ivy-switch-buffer-faces-alist)
  (push '(python-mode . swiper-match-face-2) ivy-switch-buffer-faces-alist)
  (push '(c++-mode . swiper-match-face-3) ivy-switch-buffer-faces-alist)
  (push '(rust-mode . swiper-match-face-4) ivy-switch-buffer-faces-alist)
  (ivy-mode 1))

(use-package ivy-hydra
  :straight t
  :bind (:map ivy-minibuffer-map
              ("M-o" . ivy-dispatching-done-hydra)))

(use-package ivy-prescient
  :straight t
  :after ivy
  :config
  (setq ivy-prescient-retain-classic-highlighting t)
  (ivy-prescient-mode 1))

(use-package counsel
  :straight t
  :after ivy
  :bind (("C-s" . counsel-grep-or-swiper)
         ("C-x b" . counsel-switch-buffer)
         ("C-x B" . counsel-switch-buffer-other-window)
         ("C-x y" . counsel-yank-pop)
         ("C-x C-r" . counsel-recentf)
         ("M-i" . counsel-imenu)
         ("C-c a" . my/counsel-rg)
         ("C-c u" . counsel-unicode-char)
         ("<S-f10>" . counsel-org-goto-all)
         :map counsel-mode-map
         ([remap pop-to-mark-command] . nil)
         ([remap describe-key] . nil)
         ([remap describe-function] . nil)
         ([remap describe-variable] . nil)
         ([remap describe-symbol] . nil))
  :config
  (setq counsel-find-file-ignore-regexp (concat
                                         ;; file names beginning with # or .
                                         "\\(?:\\`[#.]\\)"
                                         ;; file names ending with # or ~
                                         "\\|\\(?:[#~]\\'\\)")
        counsel-grep-base-command "rg -S --no-heading --line-number --color never %s %s")
  (counsel-mode 1))

(use-package swiper
  :straight t
  :defer)

(use-package cua-base
  :bind (:map cua-global-keymap
              ("C-<return>" . nil))
  :demand
  :config
  (setq cua-enable-cua-keys nil)
  (cua-mode 1))

(use-package paren
  :config (show-paren-mode 1))

(use-package flycheck
  :straight t
  :bind ("M-<f8>" . flycheck-list-errors)
  :hook ((prog-mode . flycheck-mode) (flycheck-mode . my/use-eslint-from-node-modules))
  :config
  (setq flycheck-indication-mode 'left-fringe
        flycheck-emacs-lisp-load-path 'inherit
        flycheck-flake8rc ".flake8")
  (flycheck-add-next-checker 'python-flake8 'python-pylint)
  (when window-system
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      (vector #b00001000
              #b00001100
              #b00001110
              #b00001111
              #b00001110
              #b00001100
              #b00001000))))

(use-package js
  :config
  (setq js-switch-indent-offset js-indent-level))

(use-package flyspell
  :hook (rst-mode . flyspell-mode))

(use-package prog-mode
  :hook (prog-mode . my/prog-mode-hook))

(use-package auctex
  :straight auctex
  :defer
  :hook (LaTeX-mode . my/latex-mode-hook)
  :init (use-package preview
          :defer
          :config
          (nconc preview-default-preamble
                 '("\\PreviewEnvironment{enumerate}" "\\PreviewEnvironment{itemize}" "\\PreviewEnvironment{description}" "\\PreviewEnvironment{tabular}"
                   "\\PreviewEnvironment{flushleft}"))
          (setq preview-auto-cache-preamble nil))
  :config
  (setq TeX-clean-confirm nil
        TeX-save-query nil)
  (setq-default TeX-engine 'xetex))

(use-package macrostep
  :straight t
  :bind (:map emacs-lisp-mode-map
              ("C-c e" . macrostep-expand)))

(use-package suggest
  :straight t
  :defer)

(use-package cc-mode
  :mode ("\\.x\\'" . c++-mode)
  :bind (:map c-mode-base-map
              ("C-c o" . ff-get-other-file)
              ("C-c f" . my/maybe-clang-format-buffer)
              ("C-c i a" . my/insert-all-special)
              ("C-c i c" . my/insert-default-ctor)
              ("C-c i d" . my/insert-virtual-dtor)
              ("C-c i p" . my/insert-copy-ctor)
              ("C-c i P" . my/insert-copy-assignment-operator)
              ("C-c i m" . my/insert-move-ctor)
              ("C-c i M" . my/insert-move-assignment-operator)
         :map java-mode-map
              ("C-c j" . my/show-in-intellij))
  :hook (c-mode-common . my/c-mode-common-hook)
  :hook (c-mode-common . lsp)
  :config (setq c-basic-offset 4
                c-default-style "bsd"))

(use-package clang-format
  :straight t
  :defer)

(use-package python
  :mode (("SCons\\(truct\\|cript\\)\\'" . python-mode)
         ("slashrc\\'" . python-mode))
  :bind (:map python-mode-map
              ("C-<f8>" . my/pylint-ignore-errors-at-point)
              ("C-c C-f" . nil)
              ("C-c i" . my/python-insert-import)
              ("C-c I" . my/py-isort-buffer))
  :hook ((python-mode . lsp)
         (python-mode . my/set-python-write-functions))
  :config
  (advice-add #'python-indent-shift-left :around #'my/python-shift-region)
  (advice-add #'python-indent-shift-right :around #'my/python-shift-region))

(use-package blacken
  :straight t
  :defer)

(use-package pyvenv
  :straight t
  :init (add-hook 'pyvenv-post-activate-hooks #'lsp)
  :hook (hack-local-variables . my/pyvenv-activate))

(use-package lsp-java
  :straight t
  :bind (:map java-mode-map
              ("C-c l i" . lsp-java-add-import)
              ("C-c l o" . lsp-java-organize-imports))
  :after lsp-mode)

(use-package go-mode
  :straight t
  :defer)

(use-package rust-mode
  :straight t
  :hook (rust-mode . my/rust-mode-hook)
  :bind (:map rust-mode-map
              ("C-c C-f" . nil)
              ("C-c P" . rust-promote-module-into-dir)
              ("C-c m" . my/rust-toggle-mut)
              ("C-c d" . my/dbg-wrap-or-unwrap))
  :config (setq rust-format-on-save t))

(use-package cargo
  :straight t
  :hook ((rust-mode . cargo-minor-mode)
         (conf-toml-mode . my/cargo-toml-mode)
         (magit-mode . my/magit-mode-cargo))
  :config (setq cargo-process--command-check (concat "check " my/cargo-check-flags)
                cargo-process--command-clippy (concat "clippy " my/cargo-check-flags)
                cargo-process--command-test (concat "test " my/cargo-build-flags)
                cargo-process--command-build (concat "build " my/cargo-build-flags)))

(use-package flycheck-rust
  :straight t
  :after rust-mode
  :hook (flycheck-mode . flycheck-rust-setup)
  :config (flycheck-add-next-checker 'rust-cargo 'rust-clippy))

(use-package lsp
  :straight lsp-mode
  :commands lsp
  :config
  (setq lsp-prefer-flymake nil
        lsp-restart 'ignore)
  (require 'lsp-clients))

(use-package company-lsp
  :straight t
  :after company
  :config (push 'company-lsp company-backends))

(use-package lsp-ui
  :straight t
  :hook (lsp-mode . lsp-ui-mode)
  :bind (:map lsp-mode-map
              ("C-c l r" . lsp-rename)
              ("C-c l c" . lsp-ui-sideline-apply-code-actions)
              ("C-c l f" . lsp-format-buffer)
              ("C-c l s" . lsp-ui-find-workspace-symbol)
              ("C-c l d" . lsp-ui-doc-mode)
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references))
  :config
  (setq lsp-ui-sideline-ignore-duplicate t
        lsp-ui-doc-enable nil
        lsp-ui-sideline-show-hover nil)
  (set-face-attribute 'lsp-ui-sideline-code-action nil :foreground (doom-color 'cyan))
  )

(use-package yaml-mode
  :straight t
  :defer)

(use-package cmake-font-lock
  :straight t
  :defer)

(use-package cmake-mode
  :straight t
  :hook (cmake-mode . cmake-font-lock-activate))

(use-package hippie-exp
  :bind ("M-/" . hippie-expand)
  :config (setq hippie-expand-try-functions-list '(yas-hippie-try-expand
                                                   try-expand-dabbrev
                                                   try-expand-dabbrev-all-buffers
                                                   try-expand-dabbrev-from-kill
                                                   try-complete-file-name-partially
                                                   try-complete-file-name
                                                   try-expand-all-abbrevs
                                                   try-complete-lisp-symbol-partially
                                                   try-complete-lisp-symbol
                                                   )))

(use-package avy
  :straight t
  :bind (("s-s" . avy-goto-word-or-subword-1)
         ("C-s-s" . avy-goto-char))
  :config (setq avy-style 'words))

(use-package company
  :straight t
  :bind (:map company-mode-map ("C-c TAB" . company-complete))
  :config
  (setq company-minimum-prefix-length 2
        company-backends (delete 'company-clang company-backends)
        company-global-modes '(not eshell-mode)
        company-tooltip-align-annotations t)
  (global-company-mode 1))

(use-package company-dabbrev
  :defer
  :config (setq company-dabbrev-downcase nil
                company-dabbrev-ignore-case t))

(use-package company-prescient
  :straight t
  :config (company-prescient-mode 1))

(use-package conf-mode
  :mode "\\.pylintrc\\'"
  :mode ("Pipfile\\'" . conf-toml-mode)
  :hook (conf-mode . my/conf-mode-hook))

(use-package deadgrep
  :straight t
  :bind ("<f6>" . deadgrep))

(use-package diff-hl
  :straight t
  :demand
  :bind ("C-]" . my/hydra-diff-hl/body)
  :hook ((dired-mode . diff-hl-dired-mode) (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (defhydra my/hydra-diff-hl ()
    ("]" diff-hl-next-hunk "next")
    ("[" diff-hl-previous-hunk "previous")
    ("r" diff-hl-revert-hunk "revert")
    ("q" nil "quit"))
  (setq diff-hl-fringe-bmp-function
        (lambda (type _pos) (if (eq type 'delete) 'diff-hl-bmp-delete 'diff-hl-bmp-change)))
  (defun diff-hl-define-bitmaps ()
    (unless (fringe-bitmap-p 'diff-hl-bmp-delete)
      (define-fringe-bitmap 'diff-hl-bmp-delete
        (vector #b10000000
                #b11000000
                #b11100000
                #b11110000)
        nil nil 'bottom)
      (define-fringe-bitmap 'diff-hl-bmp-change
        (vector #b11100000)
        nil nil '(center t))))
  (global-diff-hl-mode 1))

(use-package discover-my-major
  :straight t
  :bind ("C-h <return>" . discover-my-major))

(use-package dockerfile-mode
  :straight t
  :defer)

(use-package drag-stuff
  :straight t
  :bind (("M-S-<up>" . drag-stuff-up)
         ("M-S-<down>" . drag-stuff-down)
         ("M-S-<left>" . drag-stuff-left)
         ("M-S-<right>" . drag-stuff-right))
  :hook (drag-stuff-after-drag . my/indent-line-or-region))

(use-package easy-kill
  :straight t
  :bind ([remap kill-ring-save] . easy-kill))

(use-package emmet-mode
  :straight t
  :hook (sgml-mode web-mode)
  :config (setq emmet-indentation 2
                emmet-preview-default nil))

(use-package expand-region
  :straight t
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)))

(use-package eyebrowse
  :straight t
  :config
  (setq eyebrowse-wrap-around t
        eyebrowse-new-workspace t)
  (eyebrowse-mode 1))

(use-package gitignore-mode
  :straight t
  :defer)

(use-package helpful
  :straight t
  :bind (([remap describe-key] . helpful-key)
         ([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-symbol] . helpful-symbol)))

(use-package highlight-symbol
  :straight t
  :bind (("C-\"" . highlight-symbol-at-point)
         ("C-," . highlight-symbol-prev)
         ("C-." . highlight-symbol-next))
  :config (setq highlight-symbol-colors '("highlight")))

(use-package highlight-indent-guides
  :straight t
  :hook (prog-mode . highlight-indent-guides-mode)
  :config (setq highlight-indent-guides-method 'character
                highlight-indent-guides-responsive 'stack))

(use-package volatile-highlights
  :straight t
  :config (volatile-highlights-mode 1))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config
  (setq ibuffer-expert t
        ibuffer-formats '((mark modified read-only " "
                                (name 25 25 :left :elide) " "
                                (size 6 -1 :right) " "
                                (mode 10 10 :left :elide) " "
                                (filename-and-process -1 60 :left :elide))
                          (mark " " (name 30 -1)
                                " " filename)))
  (add-hook 'ibuffer-mode-hook (apply-partially #'ibuffer-switch-to-saved-filter-groups "default")))

(use-package ibuf-ext
  :after ibuffer
  :config (setq ibuffer-show-empty-filter-groups nil
                ibuffer-saved-filter-groups '(("default"
                                               ("Dired" (mode . dired-mode))
                                               ("Rust" (mode . rust-mode))
                                               ("C/C++" (or
                                                         (mode . c-mode)
                                                         (mode . c++-mode)))
                                               ("Python" (mode . python-mode))
                                               ("Go" (mode . go-mode))
                                               ("Elisp" (mode . emacs-lisp-mode))
                                               ("Web" (or
                                                       (mode . sgml-mode)
                                                       (mode . web-mode)
                                                       (mode . css-mode)
                                                       (mode . js-mode)))
                                               ("Docs" (or
                                                        (mode . TeX-mode)
                                                        (derived-mode . markdown-mode)
                                                        (mode . org-mode)
                                                        (mode . rst-mode)))
                                               ("Git" (derived-mode . magit-mode))
                                               ("Misc" (name . "^\\*"))
                                               ))))

(use-package magit
  :straight t
  :bind (("<f9>" . magit-status)
         ("S-<f9>" . magit-log-buffer-file)
         ("C-<f9>" . magit-blame-addition)
         ("C-c g" . magit-dispatch)
         :map magit-status-mode-map
         ("<C-up>" . magit-section-backward)
         ("<C-down>" . magit-section-forward)
         ("<C-M-up>" . magit-section-backward-sibling)
         ("<C-M-down>" . magit-section-forward-sibling))
  :config
  (set-face-attribute 'magit-branch-remote nil :foreground (doom-color 'magenta))
  (setq magit-bury-buffer-function 'magit-mode-quit-window
        magit-repository-directories '(("~/dev" . 1))
        magit-diff-refine-hunk t
        magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1
        magit-section-initial-visibility-alist '((recent . show)
                                                 (unpushed . show)
                                                 (unpulled . show))
        magit-save-repository-buffers 'dontask
        magit-status-initial-section '(((unstaged) (status)) ((untracked) (status)))
        magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))
  (remove-hook 'magit-pre-display-buffer-hook #'magit-save-window-configuration)
  (magit-add-section-hook 'magit-status-sections-hook
                          #'magit-insert-recent-commits
                          #'magit-insert-unpushed-to-upstream-or-recent
                          'replace)
  (magit-add-section-hook 'magit-status-sections-hook #'magit-insert-modules-overview nil 'append)
  (magit-define-popup-option 'magit-push-popup ?o "Set push option" "--push-option="))

(use-package git-timemachine
  :straight t
  :bind ("M-<f9>" . git-timemachine))

(use-package smerge-mode
  :bind (:map smerge-mode-map
              ("C-c m" . my/hydra-smerge/body))
  :config
  (defhydra my/hydra-smerge ()
    ("a" smerge-keep-all "keep all")
    ("b" smerge-keep-base "keep base")
    ("l" smerge-keep-lower "keep lower")
    ("u" smerge-keep-upper "keep upper")
    ("r" smerge-refine "refine diff")
    ("p" smerge-prev "previous conflict")
    ("n" smerge-next "next conflict")
    ("q" nil "quit")))

(use-package magit-gitflow
  :straight t
  :hook (magit-mode . turn-on-magit-gitflow))

(use-package forge
  :straight t
  :after magit
  :config
  (push '("git.infinidat.com" "git.infinidat.com/api/v4" "git.infinidat.com" forge-gitlab-repository) forge-alist))

(use-package git-commit
  :config
  (setq git-commit-summary-max-length fill-column)
  (global-git-commit-mode 1))

(use-package git-link
  :straight t
  :bind (("C-c G h" . git-link-homepage)
         ("C-c G c" . git-link-commit)
         ("C-c G l" . git-link)
         ("C-c G H" . my/git-link-homepage-in-browser))
  :config
  (setq git-link-use-commit t)
  (push '("git\\.infinidat\\.com" git-link-gitlab) git-link-remote-alist)
  (push '("git\\.infinidat\\.com" git-link-commit-github) git-link-commit-remote-alist))

(use-package man
  :bind ("<f1>" . man)
  :config
  (set-face-attribute 'Man-overstrike nil :inherit 'font-lock-keyword-face)
  (setq Man-notify-method 'pushy))

(use-package markdown-mode
  :straight t
  :mode ("README\\.md\\'" . gfm-mode)
  :hook ((markdown-mode . auto-fill-mode)
         (markdown-mode . flyspell-mode))
  :config (setq markdown-command "cmark"))

(use-package sh-script
  :mode ("PKGBUILD\\'" . shell-script-mode))

(use-package fish-mode
  :straight t
  :defer)

(use-package multiple-cursors
  :straight t
  :bind (("C-|" . mc/edit-lines)
         ("C-;" . mc/mark-all-like-this-dwim)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-:" . my/hydra-multiple-cursors/body))
  :config (defhydra my/hydra-multiple-cursors ()
            "
^Up^           ^Down^         ^Miscellaneous^
---------------------------------------------
_p_:   Next    _n_:   Next    _l_: Edit lines
_P_:   Skip    _N_:   Skip    _a_: Mark all
_M-p_: Unmark  _M-n_: Unmark  _q_: Quit"
            ("l" mc/edit-lines :exit t)
            ("a" mc/mark-all-like-this-dwim :exit t)
            ("n" mc/mark-next-like-this)
            ("N" mc/skip-to-next-like-this)
            ("M-n" mc/unmark-next-like-this)
            ("p" mc/mark-previous-like-this)
            ("P" mc/skip-to-previous-like-this)
            ("M-p" mc/unmark-previous-like-this)
            ("q" nil)))

(use-package projectile
  :straight t
  :demand
  :bind (("<f7>" . projectile-compile-project)
         ("<C-f7>" . projectile-test-project)
         ("<S-f7>" . projectile-run-project)
         ("H-p" . projectile-switch-project)
         ("H-f" . projectile-find-file)
         :map projectile-mode-map
         ("C-c p" . projectile-command-map))
  :config
  (projectile-register-project-type 'rust-cargo '("Cargo.toml")
                                    :compile (concat "cargo check " my/cargo-check-flags)
                                    :test (concat "cargo test " my/cargo-build-flags))
  (setq projectile-completion-system 'ivy
        projectile-current-project-on-switch 'move-to-end)
  (fset #'projectile-kill-buffers #'my/projectile-kill-buffers)
  (projectile-register-project-type 'emacs-configuration '("init.el")
                                    :run "emacs --debug-init")
  (projectile-mode 1))

(use-package counsel-projectile
  :straight t
  :demand
  :bind (("C-c C-f" . counsel-projectile-find-file))
  :config
  (counsel-projectile-modify-action 'counsel-projectile-switch-project-action
                                    '((default counsel-projectile-switch-project-action-find-file)))
  (cl-delete-if (lambda (x) (string= (car x) "si")) counsel-projectile-key-bindings)
  (push '("s" . counsel-projectile-rg) counsel-projectile-key-bindings)
  (ivy-set-actions 'counsel-projectile-switch-project
                   '(("s" counsel-projectile-switch-project-action-rg "search project with rg")))
  (counsel-projectile-mode 1))

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode)
  :config (setq rainbow-delimiters-max-face-count 1))

(use-package rainbow-mode
  :straight t
  :hook ((css-mode scss-mode) . rainbow-mode)
  :config (setq rainbow-x-colors nil))

(use-package restclient
  :straight t
  :mode ("\\.http\\'" . restclient-mode))

(use-package syntax-subword
  :straight t
  :config (global-syntax-subword-mode 1))

(use-package systemd
  :straight t
  :defer)

(use-package undo-tree
  :straight t
  :config (global-undo-tree-mode 1))

(use-package web-mode
  :straight t
  :mode "\\.hbs\\'"
  :mode "\\.html\\'"
  :config (setq web-mode-code-indent-offset 2
                web-mode-markup-indent-offset 2
                web-mode-css-indent-offset 2
                web-mode-style-padding 2
                web-mode-script-padding 2
                web-mode-enable-auto-expanding t
                web-mode-enable-current-element-highlight t))

(use-package visual-regexp
  :straight t
  :defer)

(use-package js2-mode
  :straight t
  :config
  (setq js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil)
  :mode "\\.js\\'")

(use-package window-numbering
  :straight t
  :config (window-numbering-mode 1))

(use-package winner
  :config (winner-mode))

(use-package which-key
  :straight t
  :config
  (setq which-key-idle-delay 0.5)
  (which-key-mode 1))

(use-package whitespace-cleanup-mode
  :straight t
  :config (global-whitespace-cleanup-mode 1))

(use-package wrap-region
  :straight t
  :config
  (wrap-region-add-wrapper "|" "|" nil 'rust-mode)
  (wrap-region-global-mode 1))

(use-package langtool
  :straight t
  :bind (:map text-mode-map
              ("C-c l" . langtool-check)))

(use-package yasnippet
  :straight t
  :straight yasnippet-snippets
  :demand
  :bind (:map yas-minor-mode-map
              ("TAB" . nil)
              ("<tab>" . nil))
  :config
  (setq yas-prompt-functions '(yas-completing-prompt) ; use normal completion
        yas-verbosity 1)
  (yas-global-mode 1))

(use-package ivy-yasnippet
  :straight t
  :bind ("C-M-/" . ivy-yasnippet))

(use-package auto-yasnippet
  :straight t
  :bind (("C-S-w" . aya-create)
         ("C-S-y" . aya-expand)))

(use-package dumb-jump
  :straight t
  :bind (("M-g M-o" . dumb-jump-quick-look)))

(use-package prettier-js
  :straight t
  :hook (js2-mode . prettier-js-mode))

(use-package engine-mode
  :straight t
  :defer
  :config
  (engine-mode 1)
  (defengine google
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
    :keybinding "g")
  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :keybinding "d")
  (defengine stack-overflow
    "https://stackoverflow.com/search?q=%s"
    :keybinding "s")
  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s"
    :keybinding "h"))

(use-package bm
  :straight t
  :bind (("<f2>" . bm-next)
         ("S-<f2>" . bm-show)
         ("C-<f2>" . bm-toggle))
  :config (setq bm-cycle-all-buffers t
                bm-in-lifo-order t))

(use-package jump-char
  :straight t
  :bind (("C-f" . jump-char-forward)
         ("C-b" . jump-char-backward)))

(use-package powershell
  :straight t
  :defer)

(use-package savehist
  :config (savehist-mode 1))

(let ((straight-current-profile 'site))
  (my/load-if-exists (concat my/site-config-directory "config.el")))

;;; init.el ends here
