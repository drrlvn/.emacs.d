;;; config-defuns.el --- custom functions, macros and advices -*- lexical-binding: t; byte-compile-warnings: (not free-vars unresolved) -*-
;;; Commentary:
;;; Code:

;;;###autoload
(defvar-local my/python-isort-on-save nil "Format the buffer with isort before saving")

;;;###autoload
(defvar-local my/python-black-on-save nil "Format the buffer with black before saving")

;;;###autoload
(defun my/diff-current-buffer-with-file ()
  "View the differences between current buffer and its associated file."
  (interactive)
  (if (buffer-modified-p)
      (diff-buffer-with-file (current-buffer))
    (message "Buffer not modified")))

;;;###autoload
(defun my/revert-buffer-no-confirmation ()
  "Invoke `revert-buffer' without the confirmation."
  (interactive)
  (revert-buffer nil 'noconfirm)
  (message "Reverted buffer %s" buffer-file-name))

;;;###autoload
(defun my/kill-buffer-other-window ()
  "Kill buffer in other window."
  (interactive)
  (kill-buffer (window-buffer (next-window))))

;;;###autoload
(defun my/counsel-rg ()
  "."
  (interactive)
  (counsel-rg nil default-directory))

(defun my/insert-dbg ()
  "Insert the dbg! macro."
  (insert-parentheses (unless (region-active-p) 1))
  (backward-char 1)
  (insert "dbg!"))

;;;###autoload
(defun my/dbg-wrap-or-unwrap ()
  "Either remove or add the dbg! macro."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (my/insert-dbg)

      (goto-char (beginning-of-thing 'symbol))
      (if (looking-at "dbg!")
          (progn
            (delete-char 4)
            (delete-pair))
        (my/insert-dbg)))))

;;;###autoload
(defun my/colorize-compilation-buffer ()
  "Colorize complication buffer."
  (when (eq major-mode 'compilation-mode)
    (ansi-color-apply-on-region compilation-filter-start (point-max))))

;;;###autoload
(defun my/toggle-comment-line-or-region (beg end)
  "Toggle comment betwen BEG and END, by default using line or region."
  (interactive
      (if mark-active
          (list (region-beginning) (region-end))
        (list (line-beginning-position) (line-beginning-position 2))))
  (comment-or-uncomment-region beg end))

;;;###autoload
(defun my/increment-number-at-point (n)
  "Increment number at point by N."
  (interactive "p")
  (let* ((bounds (bounds-of-thing-at-point 'word))
         (start (car bounds))
         (end (cdr bounds))
         (str (buffer-substring start end))
         (num (car (read-from-string str))))
    (when (numberp num)
      (delete-region start end)
      (insert (format (format "%%0%dd" (length str)) (+ n num))))))

;;;###autoload
(defun my/decrement-number-at-point (n)
  "Decrement number at point by N."
  (interactive "p")
  (my/increment-number-at-point (- n)))

;;;###autoload
(defun my/goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input."
  (interactive)
  (let ((prev-display-line-numbers-mode
         (if (and (boundp 'display-line-numbers-mode) display-line-numbers-mode) 1 -1)))
    (unwind-protect
        (progn
          (let ((display-line-numbers-type t))
            (ignore display-line-numbers-type)
            (display-line-numbers-mode 1))
          (let ((current-prefix-arg (read-number "Goto line: ")))
            (call-interactively 'goto-line)))
      (display-line-numbers-mode prev-display-line-numbers-mode))))

;;;###autoload
(defun my/narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first.  Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed.

Taken from http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html"
  (interactive "P")
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p) (narrow-to-region (region-beginning) (region-end)))
        (t (narrow-to-defun))))

;;;###autoload
(defun my/git-link-homepage-in-browser ()
  "Open the repository homepage in the browser."
  (interactive)
  (require 'git-link)
  (let ((git-link-open-in-browser t))
    (ignore git-link-open-in-browser)
    (call-interactively 'git-link-homepage)))

(defun my/region-line-beginning ()
  "Return the position of the line in which the region beginning is placed."
  (save-excursion
    (goto-char (region-beginning))
    (line-beginning-position)))

;;;###autoload
(defun my/indent-line-or-region ()
  "Indent region if it is active, otherwise indent line."
  (interactive)
  (if (region-active-p)
      (let ((start (my/region-line-beginning)))
        (indent-region start (region-end))
        (setq deactivate-mark nil))
    (indent-according-to-mode)))

;;;###autoload
(defun my/maybe-clang-format-buffer ()
  "Format buffer if project has .clang-format file."
  (interactive)
  (require 'clang-format)
  (let ((projectile-require-project-root nil))
    (ignore projectile-require-project-root)
    (when (and
           (not my/disable-clang-format-on-save)
           (not (string= clang-format-executable "clang-format"))
           (file-exists-p (expand-file-name ".clang-format" (projectile-project-root))))
      (clang-format-buffer)))
  nil)

;; C++ auto insert

(defun my/get-current-class ()
  "Return name of enclosing class."
  (save-excursion
    (search-backward-regexp "\\b\\(class\\|struct\\)\\b")
    (forward-word 2)
    (backward-word)
    (current-word)))

;;;###autoload
(defun my/insert-default-ctor ()
  "Insert default constructor."
  (interactive)
  (insert (my/get-current-class) "() = default;"))

;;;###autoload
(defun my/insert-virtual-dtor ()
  "Insert virtual destructor."
  (interactive)
  (insert "virtual ~" (my/get-current-class) "() = default;"))

;;;###autoload
(defun my/insert-copy-ctor ()
  "Insert copy constructor."
  (interactive)
  (let ((current-class (my/get-current-class)))
    (insert current-class "(const " current-class " &) = default;")))

;;;###autoload
(defun my/insert-copy-assignment-operator ()
  "Insert copy assignment operator."
  (interactive)
  (let ((current-class (my/get-current-class)))
    (insert current-class " & operator=(const " current-class " &) = default;")))

;;;###autoload
(defun my/insert-move-ctor ()
  "Insert move constructor."
  (interactive)
  (let ((current-class (my/get-current-class)))
    (insert current-class "(" current-class " &&) = default;")))

;;;###autoload
(defun my/insert-move-assignment-operator ()
  "Insert move assignment operator."
  (interactive)
  (let ((current-class (my/get-current-class)))
    (insert current-class " & operator=(" current-class " &&) = default;")))

;;;###autoload
(defun my/insert-all-special ()
  "Insert all special methods."
  (interactive)
  (my/insert-copy-ctor)
  (newline-and-indent)
  (my/insert-copy-assignment-operator)
  (newline-and-indent)
  (my/insert-move-ctor)
  (newline-and-indent)
  (my/insert-move-assignment-operator)
  (newline-and-indent)
  )

;;;###autoload
(defun my/rust-toggle-mut ()
  "Toggle mut for variable under point."
  (interactive)
  (save-excursion
    (lsp-find-definition)
    (if (looking-back "mut\\s-+" (point-at-bol))
        (delete-region (match-beginning 0) (match-end 0))
      (insert "mut "))))

;;;###autoload
(defun my/magit-status-config-project ()
  "Open `magit-status` for the configuration project."
  (interactive)
  (magit-status-setup-buffer (magit-toplevel (file-name-directory user-init-file))))

(defun my/py-isort-buffer ()
  "Run `isort' on buffer."
  (interactive)
  (let ((tmp-buf (generate-new-buffer "tmp")))
    (call-shell-region (point-min) (point-max) "isort -" nil tmp-buf)
    (with-current-buffer tmp-buf
      (delete-trailing-whitespace))
    (replace-buffer-contents tmp-buf)
    (kill-buffer tmp-buf))
  nil)

;;;###autoload
(defun my/blacken-buffer ()
  "Run `blacken-buffer' and always return nil."
  (interactive)
  (blacken-buffer)
  nil)

;;;###autoload
(defun my/python-insert-import ()
  "Move current line, which should be an import statement, to the beginning of the file and run isort."
  (interactive)
  (save-excursion
    (let ((import-string (string-trim (delete-and-extract-region (line-beginning-position) (line-end-position)))))
      (delete-char -1)
      (goto-char (point-min))
      (while (or (not (looking-at "$")) (python-syntax-comment-or-string-p))
        (forward-line))
      (insert import-string)
      (my/py-isort-buffer))))

;; hooks

;;;###autoload
(defun my/latex-mode-hook ()
  "."
  (TeX-add-symbols '("textenglish") '("texthebrew")))

;;;###autoload
(defun my/org-mode-hook ()
  "."
  (make-local-variable 'show-paren-mode)
  (setq show-paren-mode nil)
  (flyspell-mode 1))

;;;###autoload
(defun my/prog-mode-hook ()
  "."
  (setq show-trailing-whitespace t)
  (font-lock-add-keywords
   nil
   '(("\\<\\(FIXME\\|TODO\\|XXX\\|BUG\\)\\>" 1 font-lock-warning-face t))))

;;;###autoload
(defun my/c-mode-common-hook ()
  "."
  (setq comment-start "/*"
        comment-end "*/")
  (c-set-offset 'innamespace 0)
  (add-hook 'write-contents-functions #'my/maybe-clang-format-buffer))

;;;###autoload
(defun my/conf-mode-hook ()
  "."
  (setq require-final-newline t))

;;;###autoload
(defun my/rust-mode-hook ()
  "."
  (lsp)
  (let ((preferences (make-hash-table)))
      (puthash "clippy_preference" "on" preferences)
      (lsp--set-configuration `(:rust ,preferences))))

;;;###autoload
(defun my/pyvenv-activate ()
  "."
  (if (bound-and-true-p pyvenv-activate)
      (pyvenv-activate pyvenv-activate)))

;;;###autoload
(defun my/company-anaconda-setup ()
  "."
  (make-local-variable 'company-backends)
  (push 'company-anaconda company-backends))

;;;###autoload
(defun my/projectile-kill-buffers ()
  "Kill all buffers from current project."
  (interactive)
  (mapc 'kill-buffer (seq-remove #'buffer-base-buffer (projectile-project-buffers))))

;;;###autoload
(defun my/pylint-ignore-errors-at-point ()
  "Add a pylint ignore comment for the error on the current line."
  (interactive)
  (let* ((errs (flycheck-overlay-errors-in (line-beginning-position) (line-end-position)))
         (ids (delete-dups (seq-map #'flycheck-error-id errs))))
    (when (> (length ids) 0)
      (save-excursion
        (comment-indent)
        (insert "pylint: disable="
                (mapconcat 'identity ids ", "))))))

(defun my/zap-up-to-char (arg char)
  "Kill up to and including ARGth occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (read-char "Zap up to char: " t)))
  ;; Avoid "obsolete" warnings for translation-table-for-input.
  (with-no-warnings
    (if (char-table-p translation-table-for-input)
        (setq char (or (aref translation-table-for-input char) char))))
  (kill-region (point) (save-excursion (search-forward (char-to-string char) nil nil arg)
                                       (1- (point)))))

(defun my/update-file-autoloads ()
  "Update current file's autoloads and save."
  (let ((generated-autoload-file (format "%s-autoloads.el" (file-name-sans-extension buffer-file-name))))
    (ignore generated-autoload-file)
    (update-directory-autoloads ".")))

;;;###autoload
(defun my/python-shift-region (fn start end &optional count)
  "Advice around Python shift functions.
FN is the original function.  START is set interactivly to
the line in which the beginning of the mark is found.  END and
COUNT are set in the same way as the original function."
  (interactive
   (if mark-active
       (list (my/region-line-beginning) (region-end) current-prefix-arg)
     (list (line-beginning-position) (line-end-position) current-prefix-arg)))
  (apply fn start end count))

;;;###autoload
(defun my/scroll-up (n)
  "Scroll up N lines."
  (interactive "p")
  (scroll-up n))

;;;###autoload
(defun my/scroll-down (n)
  "Scroll down N lines."
  (interactive "p")
  (scroll-down n))

;;;###autoload
(defun my/use-eslint-from-node-modules ()
  "Use local eslint from node_modules before global."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

;;;###autoload
(defun my/cargo-toml-mode ()
  "A hook to run in TOML mode which activates cargo-minor-mode when necessary."
  (when (equal (buffer-name) "Cargo.toml")
    (cargo-minor-mode)))

;;;###autoload
(defun my/magit-mode-cargo ()
  "Activate cargo-mode in Magit if the project type is Rust."
  (when (eq (projectile-project-type) 'rust-cargo)
    (cargo-minor-mode)))

;;;###autoload
(defun my/set-python-write-functions ()
  "Format a Python buffer before saving according to local variables."
  (add-hook
   'hack-local-variables-hook
   (lambda ()
     (when my/python-isort-on-save
       (push #'my/py-isort-buffer write-contents-functions))

     (when my/python-black-on-save
       (push #'my/blacken-buffer write-contents-functions)))))

;;;###autoload
(defun my/revert-project-commands ()
  "Revert session defined commands back to their original state."
  (interactive)
  (let ((default-directory (projectile-compilation-dir)))
    (remhash default-directory projectile-compilation-cmd-map)
    (remhash default-directory projectile-test-cmd-map)
    (remhash default-directory projectile-run-cmd-map)))

;;;###autoload
(defun my/reload-dir-locals()
  "Reload dir local variables for the current buffer."
  (interactive)
  (hack-dir-local-variables-non-file-buffer)
  (my/revert-project-commands))

(provide 'config-defuns)

;;; Local Variables:
;;; eval: (add-hook 'write-contents-functions #'my/update-file-autoloads)
;;; End:

;;; config-defuns.el ends here
