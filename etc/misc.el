;; misc.el -- various customizations and additions
;;; This is for stuff that _isn't_ built-in to Emacs

;;; ACK
(require 'full-ack)

;;; ANNOYING ARROWS
(require 'annoying-arrows-mode)
(global-annoying-arrows-mode)

;;; AUTO COMPLETE
(require 'auto-complete)
(require 'auto-complete-config)
(setq ac-comphist-file (concat genehack/emacs-dir "tmp/ac-comphist.dat"))
(ac-config-default)
(setq ac-auto-start nil)
(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)
(setq-default ac-sources '(ac-source-dictionary
                           ac-source-abbrev
                           ac-source-words-in-buffer
                           ac-source-words-in-same-mode-buffers
                           ac-source-words-in-all-buffer))

;;; AUTO CREATE DIRECTORIES
;;;; after <http://atomized.org/2008/12/emacs-create-directory-before-saving/>
(add-hook 'before-save-hook
          '(lambda ()
             (or (file-exists-p (file-name-directory buffer-file-name))
                 (make-directory (file-name-directory buffer-file-name) t))))

;;; AUTOPAIR
(require 'autopair)

;;; BROWSE-KILL-RING
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;;; COFFEE-MODE
(require 'coffee-mode)

;;; CSS-HEXCOLOR
(require 'css-hexcolor)

;;; DIFF-CURRENT-BUFFER-WITH-FILE
(defun genehack/diff-current-buffer-with-file ()
  "Show diff between current buffer contents and file on disk"
  (interactive)
  (diff-buffer-with-file (current-buffer)))

;;; DELIM-KILL
;;;; <http://github.com/thomas11/delim-kill>
(require 'delim-kill)

;;; DIRED-RIGHT-HERE
(defun genehack/dired-right-here (arg)
  "Run ido-dired or, with prefix, dired on current active directory."
  (interactive "p")
  (if (eq 1 arg)
      (ido-dired)
    (dired default-directory)))

;;; DISK
(require 'disk)

;;; EXPAND-REGION
(require 'expand-region)

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

;;; FLYMAKE
(require 'flymake)
(require 'flymake-cursor)
(setq flymake-no-changes-timeout 5)
(setq temporary-file-directory "~/.emacs.d/tmp/")
(setq flymake-run-in-place nil)
;; I want to see all errors for the line.
(setq flymake-number-of-errors-to-display nil)
(set-face-foreground 'flymake-warnline "DarkBlue")
(set-face-background 'flymake-warnline "LightGray")

;;; GIT BLAME FOR LINE
(defun genehack/git-blame-for-line ()
  (interactive)
  (let ((blame-line (line-number-at-pos (point)))
        (blame-file (buffer-file-name)))
    (setq blame-out (shell-command-to-string (format "~/bin/git-blame-from-line-num %s %s" blame-line blame-file)))
    (with-output-to-temp-buffer "*git blame*" (princ blame-out))))

;;; HTML TIDY
(autoload 'tidy-buffer             "tidy" "Run Tidy HTML parser on current buffer" t)
(autoload 'tidy-parse-config-file  "tidy" "Parse the `tidy-config-file'"           t)
(autoload 'tidy-save-settings      "tidy" "Save settings to `tidy-config-file'"    t)
(autoload 'tidy-build-menu         "tidy" "Install an options menu for HTML Tidy." t)

;;; IDO-UBI
;;;; from http://whattheemacsd.com//setup-ido.el-01.html
;; Use ido everywhere
(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

;; Fix ido-ubiquitous for newer packages
(defmacro ido-ubiquitous-use-new-completing-read (cmd package)
  `(eval-after-load ,package
     '(defadvice ,cmd (around ido-ubiquitous-new activate)
        (let ((ido-ubiquitous-enable-compatibility nil))
          ad-do-it))))

(ido-ubiquitous-use-new-completing-read webjump 'webjump)
(ido-ubiquitous-use-new-completing-read yas/expand 'yasnippet)
(ido-ubiquitous-use-new-completing-read yas/visit-snippet-file 'yasnippet)

;;; JS2
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq-default js2-basic-offset 2)

;;; KILL THIS BUFFER
(defun genehack/kill-this-buffer ()
  "Kill the current buffer without prompting."
  (interactive)
  (kill-buffer nil))

;;; KOLON-MODE
(autoload 'kolon-mode "kolon-mode" "kolon-mode")
(add-to-list 'auto-mode-alist '("\\.tx" . kolon-mode))

;;; LINE NUMBERS WITH M-G
;; from http://whattheemacsd.com//key-bindings.el-01.html
(global-set-key [remap goto-line] 'goto-line-with-feedback)

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

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

(defun genehack/magit-key (arg)
  "Call magit-status, or with prefix call genehack/magit-status-with-prompt"
  (interactive "P")
  (if arg
      (call-interactively 'genehack/magit-status-with-prompt)
    (call-interactively 'magit-status)))

;;;; full screen magit-status
;;;; from http://whattheemacsd.com/setup-magit.el-01.html

(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

;;; MARKDOWN
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.mr?kd" . markdown-mode))
(add-hook 'markdown-mode-hook 'auto-complete-mode)

;;; MOVE TEXT
(require 'move-text)
(move-text-default-bindings)

;;; MULTI-TERM
(require 'multi-term)
(defalias 'term 'multi-term)
(custom-set-variables
 '(term-default-bg-color "#000000")
 '(term-default-fg-color "#cccccc"))
(setq multi-term-dedicated-select-after-open-p t
      multi-term-dedicated-window-height 24)

;;; NOOP
(defun genehack/noop nil "..." (interactive))

;;; OPEN LINE
;;;; from http://whattheemacsd.com//editing-defuns.el-01.html
(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

;;; PAREN-BOUNCE
;;;; ganked from <http://elfs.livejournal.com/1216037.html>
(defun genehack/paren-bounce ()
  (interactive)
  (let ((prev-char (char-to-string (preceding-char)))
        (next-char (char-to-string (following-char))))
    (cond ((string-match "[[{(<]" next-char) (forward-sexp 1))
          ((string-match "[\]})>]" prev-char) (backward-sexp 1))
          (t (error "%s" "Not an expression boundary.")))))

;;; POWERLINE
(require 'powerline)
(powerline-default)

;;; PROJECTILE
(require 'projectile)
(projectile-global-mode)
(setq projectile-cache-file ".projectile.cache")

;;; SAVE-AND-KILL
(defun genehack/save-and-kill ()
  "Save current buffer and then kill it."
  (interactive)
  (if (and (buffer-file-name)
           (buffer-modified-p))
      (save-buffer))
  (if (or (string-equal (buffer-name) "*scratch*")
          (string-equal (buffer-name) "*Messages*"))
      (bury-buffer)
    (kill-buffer)))

;;; SCRATCH-BUFFER
(defun genehack/create-scratch-buffer nil
  "(re)create a scratch buffer"
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (insert initial-scratch-message)
  (lisp-interaction-mode))

;;; SMART-TAB
(require 'smart-tab)
(global-smart-tab-mode 1)
(setq smart-tab-using-hippie-expand t)
(setq smart-tab-completion-functions-alist
      '((cperl-mode      . auto-complete)
        (text-mode       . dabbrev-completion)))

;;; SMEX
(require 'smex)
(smex-initialize)

;;; SPLIT-(HORIZONT|VERTIC)ALLY-OR-DELETE-OTHER-WINDOWS
(defun genehack/split-horizontally-or-delete-other-windows ()
  "If one window, split horizontally; otherwise, delete-other-windows"
  (interactive)
  (if (eq 1 (count-windows nil))
      (split-window-horizontally)
    (delete-other-windows)))

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

;;;; inspired by http://whattheemacsd.com/buffer-defuns.el-01.html
(defun genehack/strip-whitespace ()
  "Untabify, strip white space, set file coding to UTF8"
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

(defun genehack/strip-whitespace-and-indent ()
  "Strip various whitespaces and reindent whole file"
  (interactive)
  (genehack/strip-whitespace)
  (indent-region (point-min) (point-max)))

(add-hook 'before-save-hook
          (lambda ()
            (if (find major-mode genehack/strip-trailing-whitespace-in-these-modes)
                (genehack/strip-whitespace))))

;;; TEMPLATE
(require 'template-mode)
(add-hook 'html-mode-hook
          (lambda ()
            (if (string-match "\\.tt2?$" buffer-file-name)
                (template-minor-mode 1))))

;;; THEME
(require 'solarized)
(defun genehack/solarize-this ()
  "Enable solarized theme"
  (interactive)
  (load-theme 'solarized-dark t))
(defun genehack/solarize-this-light ()
  "Enable solarized theme"
  (interactive)
  (load-theme 'solarized-light t))
(require 'twilight-theme)
(defun genehack/twilight-this ()
  "Enable twilight theme"
  (interactive)
  (load-theme 'twilight t))
(genehack/solarize-this)

;;; TOGGLE-BOL
(defun genehack/bol-toggle ()
  "Toggle between beginning of indent and beginning of line"
  (interactive)
  (let ((genehack/bol-command-name "genehack/bol-toggle"))
    (setq this-command genehack/bol-command-name)
    (cond ((equal (point) (point-at-bol)) (back-to-indentation))
          ((equal last-command genehack/bol-command-name) (move-beginning-of-line nil))
          (t (back-to-indentation)))))

;;; UNICODE
(defun genehack/unicode (char)
  "Insert Unicode character at point"
  (interactive "MCharacter name? ")
  (insert (shell-command-to-string (format "u %s" char))))

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
(yas--initialize)
(setq yas-snippet-dirs (concat genehack/emacs-dir "share/snippets"))
(if (file-exists-p yas-snippet-dirs)
    (unless (file-directory-p yas-snippet-dirs)
      (error "Snippets directory creation blocked by file"))
  (make-directory yas-snippet-dirs))
(yas-load-directory yas-snippet-dirs)
(yas-global-mode)

;;;; UPDATE CURSOR COLOR BASED ON YASNIPPET STATUS
;;; http://stackoverflow.com/questions/14264228/how-can-i-trigger-an-event-when-a-yasnippet-macro-can-fire
;;; https://gist.github.com/4506396
(setq default-cursor-color "red")
(setq default-cursor-type 'bar)
(setq yasnippet-can-fire-cursor-color "green")
(setq yasnippet-can-fire-cursor-type 'box)

;; It will test whether it can expand, if yes, cursor color -> green.
(defun yasnippet-can-fire-p (&optional field)
  (interactive)
  (setq yas--condition-cache-timestamp (current-time))
  (let (templates-and-pos)
    (unless (and yas-expand-only-for-last-commands
                 (not (member last-command yas-expand-only-for-last-commands)))
      (setq templates-and-pos
            (if field
                (save-restriction
                  (narrow-to-region (yas--field-start field)
                                    (yas--field-end field))
                  (yas--current-key))
              (yas--current-key))))
    (and templates-and-pos (first templates-and-pos))))

(defun yasnippet-change-cursor-color-when-can-fire (&optional field)
  (interactive)
  (if (yasnippet-can-fire-p)
      (progn
        (set-cursor-color yasnippet-can-fire-cursor-color)
        (setq cursor-type yasnippet-can-fire-cursor-type))
    (set-cursor-color default-cursor-color)
    (setq cursor-type default-cursor-type)))

;; As pointed out by Dmitri, this will make sure it will update color when needed.
(add-hook 'post-command-hook 'yasnippet-change-cursor-color-when-can-fire)

;; put this at the end so that everything is loaded...
;;; DIMINISH
;; from http://whattheemacsd.com/init.el-04.html
(require 'diminish)
(diminish 'auto-complete-mode)
(diminish 'projectile-mode)
(diminish 'smart-tab-mode)
(diminish 'yas-minor-mode)
