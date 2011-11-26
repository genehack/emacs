;; misc.el -- various customizations and additions
;;; This is for stuff that _isn't_ built-in to Emacs

;;; ACK
(require 'ack)

;;; ANYTHING
(genehack/add-emacs-lib-subdir-to-load-path "anything")
(setq anything-command-map-prefix-key "S-<F5>")
(setq anything-c-adaptive-history-file "~/.emacs.d/tmp/anything-c-adaptive-history")
(require 'anything-config)


;;; AUTO COMPLETE
(genehack/add-emacs-lib-subdir-to-load-path "auto-complete")
(require 'auto-complete)
(require 'auto-complete-config)
(setq ac-comphist-file (concat genehack/emacs-dir "tmp/ac-comphist.dat"))
(add-to-list 'ac-dictionary-directories (concat genehack/emacs-libs-dir "auto-complete/dict"))
(ac-config-default)
(setq ac-auto-start nil)
(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)
(setq-default ac-sources '(ac-source-dictionary
                           ac-source-abbrev
                           ac-source-words-in-buffer
                           ac-source-words-in-same-mode-buffers
                           ac-source-words-in-all-buffer
                           ac-source-yasnippet))


;;; AUTO CREATE DIRECTORIES
;;;; after <http://atomized.org/2008/12/emacs-create-directory-before-saving/>
(add-hook 'before-save-hook
          '(lambda ()
             (or (file-exists-p (file-name-directory buffer-file-name))
                 (make-directory (file-name-directory buffer-file-name) t))))

;;; BROWSE-KILL-RING
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;;; COFFEE-MODE
(genehack/add-emacs-lib-subdir-to-load-path "coffee-mode")
(require 'coffee-mode)

;;; COLOR-THEME
(genehack/add-emacs-lib-subdir-to-load-path "color-theme")
(genehack/add-emacs-lib-subdir-to-load-path "solarized/emacs-color-theme-solarized")
(require 'color-theme)
(setq color-theme-load-all-themes nil)
(color-theme-initialize)
(require 'color-theme)
(require 'color-theme-twilight)
(require 'color-theme-hober2)
(require 'color-theme-tangotango)
(require 'color-theme-solarized)

;; select theme - first list element is for windowing system, second is for console/terminal
(setq color-theme-choices
      '(color-theme-solarized-dark color-theme-hober2))

(defun genehack/load-scheme (&optional frame)
  (interactive)
  (let ((color-theme-is-global nil))
    (if (framep frame)
        (select-frame frame))
    (if (window-system (selected-frame))
        (eval (list (car color-theme-choices)))
      (eval (cdr color-theme-choices)))))
(add-hook 'after-make-frame-functions 'genehack/load-scheme)

(defun genehack/solarize-this-frame ()
  (interactive)
  (let ((color-theme-is-global nil))
    ;; not sure why i have to do this twice, but the first time
    ;; doesn't get the mode line switched.
    (color-theme-solarized 'dark)
    (color-theme-solarized 'dark)))

;;; CSS-HEXCOLOR
(require 'css-hexcolor)

;;; DIFF-CURRENT-BUFFER-WITH-FILE
(defun genehack/diff-current-buffer-with-file ()
  "Show diff between current buffer contents and file on disk"
  (interactive)
  (diff-buffer-with-file (current-buffer)))

;;; DELIM-KILL
;;;; <http://github.com/thomas11/delim-kill>
(genehack/add-emacs-lib-subdir-to-load-path "delim-kill")
(require 'delim-kill)

;;; DIRED-RIGHT-HERE
(defun genehack/dired-right-here (arg)
  "Run ido-dired or, with prefix, dired on current active directory."
  (interactive "p")
  (if (eq 1 arg)
      (ido-dired)
    (dired default-directory)))

;;; DISK
(autoload 'disk "disk" "Save, revert, or find file." t)

;;; FILLADAPT -- WTF isn't this part of emacs by default by now?!
(require 'filladapt)

;;; FIXME
(require 'fixme)

;;; FLYMAKE FOR CSS FILES
;;;; after <http://www.emacswiki.org/emacs-en/FlymakeCSS>
(if (file-exists-p "/opt/css-validator")
    (progn
      (require 'flymake)
      (defconst css-validator "java -jar /opt/css-validator/css-validator.jar")

      (defun flymake-css-init ()
        (let* ((temp-file   (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))
               (local-file  (file-relative-name
                             temp-file
                             (file-name-directory buffer-file-name))))
          (list "java" (list "-jar" "/opt/css-validator/css-validator.jar" "-output" "gnu" (concat "file:" local-file)))))

      (push '(".+\\.css$" flymake-css-init) flymake-allowed-file-name-masks)
      (push '("^file:\\([^:]+\\):\\([^:]+\\):\\(.*\\)" 1 2 nil 3) flymake-err-line-patterns)

      (add-hook 'css-mode-hook 'flymake-mode)))

;;; FLYMAKE CURSOR
(require 'flymake)
(require 'flymake-cursor)

;;; HTML TIDY
(autoload 'tidy-buffer             "tidy" "Run Tidy HTML parser on current buffer" t)
(autoload 'tidy-parse-config-file  "tidy" "Parse the `tidy-config-file'"           t)
(autoload 'tidy-save-settings      "tidy" "Save settings to `tidy-config-file'"    t)
(autoload 'tidy-build-menu         "tidy" "Install an options menu for HTML Tidy." t)

;;; JS2
(genehack/add-emacs-lib-subdir-to-load-path "js2-mode")
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq-default js2-basic-offset 2)

;;; KILL THIS BUFFER
(defun genehack/kill-this-buffer ()
  "Kill the current buffer without prompting."
  (interactive)
  (kill-buffer nil))

;;; MACRO
(autoload 'macro-dwim "macro" "DWIM macro recording and playback." t)
(autoload 'macro-clear "macro" "Clear last keyboard macro" t)

;;; MAGIT
(defvar genehack/git-executable (executable-find "git")
  "Path to active git executable")

(if genehack/git-executable
    (progn
      (require 'magit)
      (defun genehack/magit-status-with-prompt (dir)
        "Prompt for git repo path then call magit-status on it."
        (interactive "Dgit repo: ")
        (magit-status dir)))
  (defun genehack/magit-status-with-prompt ()
    "Stub function for when git isn't available"
    (interactive)
    (message "Unable to find a git binary; magit is unavailable.")))

;;; MARKDOWN
(genehack/add-emacs-lib-subdir-to-load-path "markdown-mode")
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.mr?kd" . markdown-mode))
(add-hook 'markdown-mode-hook 'auto-complete-mode)

;;; MAXFRAME
(genehack/add-emacs-lib-subdir-to-load-path "maxframe")
(require 'maxframe)
(add-hook 'window-setup-hook 'maximize-frame t)

;;; MAYBE-ERC
(defun genehack/maybe-erc ()
  "Run genehack/erc or give an error"
  (interactive)
  (if (functionp 'genehack/erc)
      (genehack/erc)
    (message "ERC config is not loaded.")))

;;; MAYBE-GNUS
(defun genehack/maybe-gnus ()
  "Run genehack/gnus or give an error"
  (interactive)
  (if (functionp 'genehack/gnus)
      (genehack/gnus)
    (message "GNUS config is not loaded.")))

;;; MULTI-TERM
(require 'multi-term)
(defalias 'term 'multi-term)
(setq multi-term-dedicated-select-after-open-p t
      multi-term-dedicated-window-height 24)

;;; PACKAGE
(eval-after-load "package" '(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/")))

;;; PAREN-BOUNCE
;;;; ganked from <http://elfs.livejournal.com/1216037.html>
(defun genehack/paren-bounce ()
  (interactive)
  (let ((prev-char (char-to-string (preceding-char)))
	(next-char (char-to-string (following-char))))
    (cond ((string-match "[[{(<]" next-char) (forward-sexp 1))
          ((string-match "[\]})>]" prev-char) (backward-sexp 1))
          (t (error "%s" "Not an expression boundary.")))))

;;; SCRATCH-BUFFER
(defun genehack/create-scratch-buffer nil
  "(re)create a scratch buffer"
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (insert initial-scratch-message)
  (lisp-interaction-mode))

;;; SMART-TAB
(genehack/add-emacs-lib-subdir-to-load-path "smart-tab")
(require 'smart-tab)
(global-smart-tab-mode 1)
(setq smart-tab-completion-functions-alist
      '((cperl-mode      . auto-complete)
        (text-mode       . dabbrev-completion)))


;;; SMEX
(require 'smex)
(smex-initialize)

;;; SPLIT-VERTICALLY-OR-DELETE-OTHER-WINDOWS
(defun genehack/split-vertically-or-delete-other-windows ()
  "If one window, split vertically; otherwise, delete-other-windows"
  (interactive)
  (if (eq 1 (count-windows nil))
      (split-window-vertically)
    (delete-other-windows)))

;;; STRIP TRAILING WHITESPACE
(defvar genehack/strip-trailing-whitespace-in-these-modes
  '(
    cperl-mode
    css-mode
    emacs-lisp-mode
    tt-mode
    yaml-mode
    )
  "List of modes where trailing whitespace should be stripped when saving files.")

(add-hook 'before-save-hook
          (lambda ()
            (if (find major-mode genehack/strip-trailing-whitespace-in-these-modes)
              (delete-trailing-whitespace))))

;;; TEMPLATE
(require 'template-mode)
(add-hook 'html-mode-hook
          (lambda ()
            (if (string-match "\\.tt2?$" buffer-file-name)
                (template-minor-mode 1))))

;;; TEXTILE
(require 'textile-mode)
(add-to-list 'auto-mode-alist '("\\.textile\\'" . textile-mode))

;;; TEXTMATE
(genehack/add-emacs-lib-subdir-to-load-path "textmate")
(require 'textmate)
(textmate-mode)

;;; THEME
;;(if (custom-theme-p 'naquadah)
;;    (load-theme 'naquadah))

;;; TOGGLE-BOL
(defun genehack/bol-toggle ()
  "Toggle between beginning of indent and beginning of line"
  (interactive)
  (let ((genehack/bol-command-name "genehack/bol-toggle"))
    (setq this-command genehack/bol-command-name)
    (cond ((equal (point) (point-at-bol)) (back-to-indentation))
          ((equal last-command genehack/bol-command-name) (move-beginning-of-line nil))
          (t (back-to-indentation)))))

;;; URL ENCODING
;; based on http://twitter.com/#!/OvidPerl/status/28076709865586688
(defun genehack/unescape_uri (b e)
  (interactive "r")
  (shell-command-on-region
   b e
   "perl -MURI::Escape -e 'print URI::Escape::uri_unescape(do { local $/; <STDIN> })'"
   'current-buffer t))




;;; YAML-MODE
(autoload 'yaml-mode "yaml-mode" "YAML" t)
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))

;;; YASNIPPET
(require 'yasnippet)
(setq yas/root-directory (concat genehack/emacs-dir "share/snippets"))
(if (file-exists-p yas/root-directory)
    (unless (file-directory-p yas/root-directory)
      (error "Snippets directory creation blocked by file"))
  (make-directory yas/root-directory))
(yas/load-directory yas/root-directory)
(yas/global-mode 1)
