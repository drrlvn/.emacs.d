;;; config-defuns-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "config-custom" "config-custom.el" (0 0 0 0))
;;; Generated autoloads from config-custom.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "config-custom" '("my/")))

;;;***

;;;### (autoloads nil "config-defuns" "config-defuns.el" (0 0 0 0))
;;; Generated autoloads from config-defuns.el

(defvar-local my/python-isort-on-save nil "\
Format the buffer with isort before saving")

(defvar-local my/python-black-on-save nil "\
Format the buffer with black before saving")

(autoload 'my/diff-current-buffer-with-file "config-defuns" "\
View the differences between current buffer and its associated file.

\(fn)" t nil)

(autoload 'my/revert-buffer-no-confirmation "config-defuns" "\
Invoke `revert-buffer' without the confirmation.

\(fn)" t nil)

(autoload 'my/kill-buffer-other-window "config-defuns" "\
Kill buffer in other window.

\(fn)" t nil)

(autoload 'my/counsel-rg "config-defuns" "\
.

\(fn)" t nil)

(autoload 'my/dbg-wrap-or-unwrap "config-defuns" "\
Either remove or add the dbg! macro.

\(fn)" t nil)

(autoload 'my/colorize-compilation-buffer "config-defuns" "\
Colorize complication buffer.

\(fn)" nil nil)

(autoload 'my/toggle-comment-line-or-region "config-defuns" "\
Toggle comment betwen BEG and END, by default using line or region.

\(fn BEG END)" t nil)

(autoload 'my/increment-number-at-point "config-defuns" "\
Increment number at point by N.

\(fn N)" t nil)

(autoload 'my/decrement-number-at-point "config-defuns" "\
Decrement number at point by N.

\(fn N)" t nil)

(autoload 'my/goto-line-with-feedback "config-defuns" "\
Show line numbers temporarily, while prompting for the line number input.

\(fn)" t nil)

(autoload 'my/narrow-or-widen-dwim "config-defuns" "\
Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first.  Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed.

Taken from http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html

\(fn P)" t nil)

(autoload 'my/git-link-homepage-in-browser "config-defuns" "\
Open the repository homepage in the browser.

\(fn)" t nil)

(autoload 'my/indent-line-or-region "config-defuns" "\
Indent region if it is active, otherwise indent line.

\(fn)" t nil)

(autoload 'my/maybe-clang-format-buffer "config-defuns" "\
Format buffer if project has .clang-format file.

\(fn)" t nil)

(autoload 'my/insert-default-ctor "config-defuns" "\
Insert default constructor.

\(fn)" t nil)

(autoload 'my/insert-virtual-dtor "config-defuns" "\
Insert virtual destructor.

\(fn)" t nil)

(autoload 'my/insert-copy-ctor "config-defuns" "\
Insert copy constructor.

\(fn)" t nil)

(autoload 'my/insert-copy-assignment-operator "config-defuns" "\
Insert copy assignment operator.

\(fn)" t nil)

(autoload 'my/insert-move-ctor "config-defuns" "\
Insert move constructor.

\(fn)" t nil)

(autoload 'my/insert-move-assignment-operator "config-defuns" "\
Insert move assignment operator.

\(fn)" t nil)

(autoload 'my/insert-all-special "config-defuns" "\
Insert all special methods.

\(fn)" t nil)

(autoload 'my/rust-toggle-mut "config-defuns" "\
Toggle mut for variable under point.

\(fn)" t nil)

(autoload 'my/magit-status-config-project "config-defuns" "\
Open `magit-status` for the configuration project.

\(fn)" t nil)

(autoload 'my/blacken-buffer "config-defuns" "\
Run `blacken-buffer' and always return nil.

\(fn)" t nil)

(autoload 'my/python-insert-import "config-defuns" "\
Move current line, which should be an import statement, to the beginning of the file and run isort.

\(fn)" t nil)

(autoload 'my/latex-mode-hook "config-defuns" "\
.

\(fn)" nil nil)

(autoload 'my/org-mode-hook "config-defuns" "\
.

\(fn)" nil nil)

(autoload 'my/prog-mode-hook "config-defuns" "\
.

\(fn)" nil nil)

(autoload 'my/c-mode-common-hook "config-defuns" "\
.

\(fn)" nil nil)

(autoload 'my/conf-mode-hook "config-defuns" "\
.

\(fn)" nil nil)

(autoload 'my/rust-mode-hook "config-defuns" "\
.

\(fn)" nil nil)

(autoload 'my/pyvenv-activate "config-defuns" "\
.

\(fn)" nil nil)

(autoload 'my/company-anaconda-setup "config-defuns" "\
.

\(fn)" nil nil)

(autoload 'my/projectile-kill-buffers "config-defuns" "\
Kill all buffers from current project.

\(fn)" t nil)

(autoload 'my/pylint-ignore-errors-at-point "config-defuns" "\
Add a pylint ignore comment for the error on the current line.

\(fn)" t nil)

(autoload 'my/python-shift-region "config-defuns" "\
Advice around Python shift functions.
FN is the original function.  START is set interactivly to
the line in which the beginning of the mark is found.  END and
COUNT are set in the same way as the original function.

\(fn FN START END &optional COUNT)" t nil)

(autoload 'my/scroll-up "config-defuns" "\
Scroll up N lines.

\(fn N)" t nil)

(autoload 'my/scroll-down "config-defuns" "\
Scroll down N lines.

\(fn N)" t nil)

(autoload 'my/use-eslint-from-node-modules "config-defuns" "\
Use local eslint from node_modules before global.

\(fn)" nil nil)

(autoload 'my/cargo-toml-mode "config-defuns" "\
A hook to run in TOML mode which activates cargo-minor-mode when necessary.

\(fn)" nil nil)

(autoload 'my/magit-mode-cargo "config-defuns" "\
Activate cargo-mode in Magit if the project type is Rust.

\(fn)" nil nil)

(autoload 'my/set-python-write-functions "config-defuns" "\
Format a Python buffer before saving according to local variables.

\(fn)" nil nil)

(autoload 'my/revert-project-commands "config-defuns" "\
Revert session defined commands back to their original state.

\(fn)" t nil)

(autoload 'my/reload-dir-locals "config-defuns" "\
Reload dir local variables for the current buffer.

\(fn)" t nil)

(autoload 'my/load-if-exists "config-defuns" "\
Load FILE if it exists.

\(fn FILE)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "config-defuns" '("my/")))
(autoload 'my/set-git-email "config-defuns" "\
Set your git EMAIL for the current project.

\(fn EMAIL)" t nil)

(autoload 'my/show-in-intellij "config-defuns" "\
Move IntelliJ to the current line in the current file.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("config-looks.el") (0 0 0 0))

;;;***

(provide 'config-defuns-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; config-defuns-autoloads.el ends here
