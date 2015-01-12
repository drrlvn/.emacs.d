;;; config-packages.el --- external packages configuration
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'cl))

(package-initialize)
(my/install-packages
 'ace-jump-mode
 'anzu
 'auto-complete
 'cmake-font-lock
 'cmake-mode
 'company
 'diff-hl
 'discover-my-major
 'drag-stuff
 'easy-kill
 'emmet-mode
 'expand-region
 'flx-ido
 'flycheck
 'git-messenger
 'git-timemachine
 'go-mode
 'guide-key
 'helm
 'helm-ag
 'helm-projectile
 'helm-swoop
 'highlight-symbol
 'ido-vertical-mode
 'iedit
 'magit
 'markdown-mode
 'multiple-cursors
 'paredit
 'popwin
 'projectile
 'protobuf-mode
 'rainbow-delimiters
 'restclient
 'rust-mode
 'smex
 'undo-tree
 'web-mode
 'whitespace-cleanup-mode
 'wrap-region
 'yaml-mode
 'yasnippet
 )

;; ace-jump-mode
(global-set-key (kbd "C-`") 'ace-jump-char-mode)
(global-set-key (kbd "C-~") 'ace-jump-word-mode)

(loop for c from ?0 to ?9 do (my/add-super-char-to-ace-jump-mode 'word c))
(loop for c from ?A to ?Z do (my/add-super-char-to-ace-jump-mode 'word c))
(loop for c from ?a to ?z do (my/add-super-char-to-ace-jump-mode 'word c))
(loop for c in '(?\( ?\) ?{ ?} ?[ ?] ?< ?>
                     ?` ?~ ?! ?@ ?# ?$ ?% ?^ ?& ?* ?- ?_ ?= ?+
                     ?\\ ?| ?\; ?: ?\" ?' ?, ?. ?/ ??)
      do (my/add-super-char-to-ace-jump-mode 'char c))

;; anzu
(global-anzu-mode +1)

;; company-mode
(global-company-mode t)

;; diff-hl
(global-diff-hl-mode t)

;; discover-my-major
(global-set-key (kbd "C-h C-m") 'discover-my-major)

;; drag-stuff
(setq drag-stuff-modifier '(meta shift))

;; emmet-mode
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)

;; expand-region
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C--") 'er/contract-region)

;; git-messenger
(global-set-key (kbd "C-x v p") 'git-messenger:popup-message)
(setq git-messenger:show-detail t)

;; git-timemachine
(global-set-key (kbd "C-x v t") 'git-timemachine)

;; guide-key
(guide-key-mode t)

;; helm
(setq helm-idle-delay 0)
(setq helm-input-idle-delay 0)
(setq helm-exit-idle-delay 0)
(helm-mode t)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-r") 'helm-recentf)
(global-set-key (kbd "C-x a") 'helm-c-apropos)
(global-set-key (kbd "C-x f") 'helm-mini)
(global-set-key (kbd "C-x g") 'helm-google-suggest)
(global-set-key (kbd "C-x y") 'helm-show-kill-ring)
(global-set-key (kbd "M-X") 'helm-M-x)
(global-set-key (kbd "M-i") 'helm-semantic-or-imenu)
(global-set-key (kbd "M-s M-o") 'helm-occur)
(global-set-key (kbd "M-s m") 'helm-multi-occur)

;; helm-projectile
(helm-projectile-on)

;; helm-swoop
(global-set-key (kbd "C-S-s") 'helm-swoop)

;; ido
(ido-vertical-mode t)
(flx-ido-mode t)

;; iedit
(global-set-key (kbd "C-;") 'iedit-mode)

;; markdown-mode
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-hook 'markdown-mode-hook (lambda ()
                                (auto-fill-mode t)
                                (refill-mode t)))
(setq markdown-command "markdown_py")

;; multiple-cursors
(global-set-key (kbd "C-|") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this-dwim)
(global-set-key (kbd "C-c C->") 'mc/mark-all-like-this-dwim)

;; paredit
;; making paredit work with delete-selection-mode
(put 'paredit-forward-delete 'delete-selection 'supersede)
(put 'paredit-backward-delete 'delete-selection 'supersede)
(put 'paredit-newline 'delete-selection t)

;; popwin
(popwin-mode t)

;; projectile
(projectile-global-mode t)
(global-set-key (kbd "C-c C-f") 'helm-projectile)
(fset 'projectile-kill-buffers 'my/projectile-kill-buffers)

;; restclient
(add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))

;; smex
(global-set-key (kbd "M-x") 'smex)

;; undo-tree
(global-undo-tree-mode t)

;; web-mode
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))

;; whitespace-cleanup-mode
(global-whitespace-cleanup-mode t)

;; wrap-region
(wrap-region-global-mode t)

;; YASnippet
(setq yas-prompt-functions '(yas-completing-prompt)) ; use normal completion, which is helm in our case
(setq yas-verbosity 1)
(yas-global-mode 1)

(provide 'config-packages)
;;; config-packages.el ends here
