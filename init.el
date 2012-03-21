(add-to-list 'load-path "~/.emacs.d/packages")

; set font
(add-to-list 'default-frame-alist '(font . "Ubuntu Mono 12"))
(if (window-system)
    (progn
      (set-face-attribute 'default nil :font "Ubuntu Mono 12")
      (custom-set-faces '(minimap-font-face ((default (:height 30)) (nil nil))))
      (blink-cursor-mode 1)
      )
  )
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(magit-diff-add ((t (:inherit diff-added :foreground "blue" :weight bold))))
 '(magit-diff-del ((t (:inherit diff-removed :foreground "red3" :weight bold))))
 '(magit-item-highlight ((t (:inherit highlight :background "gray10")))))

; disable splash screen and other crap
(setq inhibit-startup-message t)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(inhibit-startup-echo-area-message (user-login-name)))
(setq initial-scratch-message nil)
; hide toolbar
(tool-bar-mode 0)

; color-theme
(add-to-list 'load-path "~/.emacs.d/packages/color-theme")
(add-to-list 'load-path "~/.emacs.d/packages/tomorrow-theme/GNU Emacs")
(require 'color-theme-tomorrow)
(color-theme-tomorrow-night-bright)

(setq x-select-enable-clipboard t)
(delete-selection-mode 1)
(setq dabbrev-case-replace nil)
(setq display-time-24hr-format t)
(setq display-time-mode t)
(setq ring-bell-function 'ignore)
(setq history-length 500)
(global-hl-line-mode 1)
(add-hook 'find-file-hook 'turn-on-visual-line-mode)
(global-linum-mode 1)
(global-auto-revert-mode 1)
(set-face-background 'hl-line "#222")
(pc-selection-mode 1)
(cua-mode 1)
; enable IDO mode
(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-ignore-buffers (cons "^\\*" ido-ignore-buffers))
; cedet
(global-ede-mode 1)
(setq semantic-default-submodes
      '(global-semantic-idle-scheduler-mode
        global-semanticdb-minor-mode
        global-semantic-stickyfunc-mode))
; enable windmove
(windmove-default-keybindings 'meta)
; misc configuration
(setq x-select-enable-clipboard t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default show-trailing-whitespace t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq max-mini-window-height 1)
(setq lazy-highlight-initial-delay 0)
(show-paren-mode 1)
(setq show-paren-delay 0)
(setq kill-whole-line t)
(setq diff-switches "-u")
(setq column-number-mode t)
(set-scroll-bar-mode 'right)
; ediff
(setq ediff-split-window-function 'split-window-horizontally)
; enable disabled features
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)

; mappings
(defun revert-buffer-no-confirmation ()
  "Invoke `revert-buffer' without the confirmation."
  (interactive)
  (revert-buffer nil t t)
  (message (concat "Reverted buffer " buffer-file-name)))
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "<f5>") 'revert-buffer-no-confirmation)
(global-set-key (kbd "<f6>") 'ack)
(global-set-key (kbd "<f7>") 'previous-error)
(global-set-key (kbd "<f8>") 'next-error)
(global-set-key (kbd "<f9>") 'magit-status)
(global-set-key (kbd "<f11>") 'delete-trailing-whitespace)
(global-set-key (kbd "C-<delete>") 'kill-word)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-SPC") 'dabbrev-expand)

(modify-syntax-entry ?_ "w" c-mode-syntax-table)
;(modify-syntax-entry ?_ "w" python-mode-syntax-table)

; C/C++
(add-hook 'c-mode-common-hook (lambda ()
                                (semantic-mode 1)
                                (subword-mode 1)
                                (local-set-key (kbd "C-c o") 'ff-find-other-file)
                                (global-set-key (kbd "C-<delete>") 'subword-kill)
                                (font-lock-add-keywords
                                 nil
                                 '(("\\<\\(FIXME\\|TODO\\|XXX\\|BUG\\):" 1 font-lock-warning-face t)))
                                (add-hook 'local-write-file-hooks
                                          (lambda ()
                                            (save-excursion
                                              (delete-trailing-whitespace))))))
(setq c-default-style "linux"
      c-basic-offset 4)
(add-to-list 'auto-mode-alist '("\\.x\\'" . c++-mode))

; Python
(add-hook 'python-mode-hook (lambda ()
                              (semantic-mode 1)
                              (subword-mode 1)
                              (global-set-key (kbd "C-<delete>") 'subword-kill)
                              (flymake-mode)
                              (font-lock-add-keywords
                               nil
                               '(("\\<\\(FIXME\\|TODO\\|XXX\\|BUG\\):" 1 font-lock-warning-face t)))
                              (add-hook 'local-write-file-hooks
                                        (lambda ()
                                          (save-excursion
                                            (delete-trailing-whitespace))))))
(require 'flymake)
(defun flymake-pylint-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "epylint" (list local-file))))

(add-to-list 'flymake-allowed-file-name-masks '("\\.py\\'" flymake-pylint-init))

(global-set-key (kbd "S-<f7>") (lambda ()
                                 (interactive)
                                 (flymake-goto-prev-error)
                                 (message "%s"
                                          (flymake-ler-text (caar (flymake-find-err-info
                                                                   flymake-err-info
                                                                   (flymake-current-line-no)))))))
(global-set-key (kbd "S-<f8>") (lambda ()
                                 (interactive)
                                 (flymake-goto-next-error)
                                 (message "%s"
                                          (flymake-ler-text (caar (flymake-find-err-info
                                                                   flymake-err-info
                                                                   (flymake-current-line-no)))))))
(add-to-list 'load-path "~/.emacs.d/packages/markdown-mode")
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("SCons\\(truct\\|cript\\)\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

; recentf and ido-recentf
(require 'recentf)
(recentf-mode t)
(setq recentf-max-saved-items 250)
(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

(require 'tramp)

(autoload 'iedit-mode "iedit" nil t)
(define-key global-map (kbd "C-;") 'iedit-mode)

(add-to-list 'load-path "~/.emacs.d/packages/smex")
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator ":")

; anything
(add-to-list 'load-path "~/.emacs.d/packages/anything-config")
(setq anything-command-map-prefix-key "<f1>")
(require 'anything-match-plugin)
(require 'anything-config)
(defun custom-anything ()
  (interactive)
  (anything-other-buffer
   '(anything-c-source-buffers+
     anything-c-source-recentf
     anything-c-source-files-in-current-dir+
     anything-c-source-occur
     anything-c-source-imenu
     anything-c-source-semantic
     anything-c-source-emacs-commands
     )
   " *custom-anything*"))
(global-set-key [(control x) (a)] 'custom-anything)
(global-set-key (kbd "M-i") 'anything-imenu)
(global-set-key (kbd "C-x f") 'anything-find-files)

; drag-stuff
(add-to-list 'load-path "~/.emacs.d/packages/drag-stuff")
(require 'drag-stuff)
(setq drag-stuff-modifier '(meta shift))
(drag-stuff-global-mode t)

; saveplace
(require 'saveplace)
(setq-default save-place t)

; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)
(setq ibuffer-expert t)
(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-saved-filter-groups
      '(("default"
         ("Dired" (mode . dired-mode))
         ("C/C++" (or
                   (mode . c-mode)
                   (mode . c++-mode)))
         ("Python" (mode . python-mode))
         ("emacs" (name . "^\\*"))
         )))
(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-switch-to-saved-filter-groups "default")))

; ace-jump-mode
(add-to-list 'load-path "~/.emacs.d/packages/ace-jump-mode")
(autoload 'ace-jump-mode "ace-jump-mode" nil t)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

; magit
(add-to-list 'load-path "~/.emacs.d/packages/magit")
(autoload 'magit-status "magit" nil t)

; minimap
(add-to-list 'load-path "~/.emacs.d/packages/minimap")
(autoload 'minimap-create "minimap" nil t)
(setq minimap-update-delay 0.1)
(setq minimap-width-fraction 0.1)

; YASnippet
(add-to-list 'load-path "~/.emacs.d/packages/yasnippet")
(require 'yasnippet)
(yas/global-mode 1)

; zencoding
(add-to-list 'load-path "~/.emacs.d/packages/zencoding")
(autoload 'zencoding-mode "zencoding-mode" nil t)
(add-hook 'sgml-mode-hook 'zencoding-mode)

; full-ack
(add-to-list 'load-path "~/.emacs.d/packages/full-ack")
(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(autoload 'ack-find-same-file "full-ack" nil t)
(autoload 'ack-find-file "full-ack" nil t)
