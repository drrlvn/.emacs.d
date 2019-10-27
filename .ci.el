(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(push (concat default-directory "lisp") load-path)
(require 'straight-bootstrap)
(require 'straight)
(straight-use-package 'use-package)
