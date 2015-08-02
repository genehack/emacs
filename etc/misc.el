;; misc.el --- various customizations and additions
;;; Commentary:
;;; This is for stuff that _isn't_ built-in to Emacs

;;; Code:

(require 'use-package)

;;; AG
(use-package ag
  :ensure ag
  :init (setq ag-highlight-search t))

;;; AUTO CREATE DIRECTORIES
;;;; after <http://atomized.org/2008/12/emacs-create-directory-before-saving/>
(defun genehack/set-up-before-save-hook ()
  "My customizations for 'before-save-hook'."
  (or (file-exists-p (file-name-directory buffer-file-name))
      (make-directory (file-name-directory buffer-file-name) t)))
(add-hook 'before-save-hook 'genehack/set-up-before-save-hook)

;;; BROWSE-KILL-RING
(use-package browse-kill-ring
  :commands browse-kill-ring-default-keybindings
  :ensure browse-kill-ring
  :init (browse-kill-ring-default-keybindings))

;;; COMPANY-MODE
;; inspired by https://gist.github.com/nonsequitur/265010
(defun genehack/company-yasnippet-or-completion ()
  "Expand yasnippet if available, otherwise autocomplete."
  (interactive)
  (if (first (yas--templates-for-key-at-point))
      (progn (company-abort)
             (yas-expand))
    (company-complete-common)))

(use-package company
  :bind ("\t" . genehack/company-yasnippet-or-completion)
  :commands global-company-mode
  :config (progn
            (setq company-echo-delay 0
                  company-idle-delay 0.3
                  company-minimum-prefix-length 1)
            (setq-default company-backends
                          '((company-capf :with company-yasnippet)
                            (company-dabbrev-code company-keywords)
                            company-go
                            company-nxml
                            company-css
                            company-files
                            company-dabbrev)))
  :diminish company-mode
  :ensure company
  :init (global-company-mode))

;;; CONVERT LINE ENDINGS
;;;; from http://www.emacswiki.org/emacs/EndOfLineTips
(add-hook 'find-file-hook 'genehack/find-file-check-line-endings)
(defun genehack/dos-file-endings-p ()
  "Predicate for whether current buffer is in DOS mode."
  (string-match "dos" (symbol-name buffer-file-coding-system)))
(defun genehack/find-file-check-line-endings ()
  "Convert DOS file to Unix."
  (when (genehack/dos-file-endings-p)
    (set-buffer-file-coding-system 'undecided-unix)
    (set-buffer-modified-p nil)))

;;; CSS-HEXCOLOR
(use-package css-hexcolor
  :ensure genehack-misc-elisp)

;;; DIFF-CURRENT-BUFFER-WITH-FILE
(defun genehack/diff-current-buffer-with-file ()
  "Show diff between current buffer contents and file on disk."
  (interactive)
  (diff-buffer-with-file (current-buffer)))

;;; DIMINISH
;;;; from http://whattheemacsd.com/init.el-04.html
(use-package diminish
  :ensure diminish)

;;; DIRED-RIGHT-HERE
(defun genehack/dired-right-here (arg)
  "Run 'ido-dired' or, with ARG, 'dired' on current active directory."
  (interactive "p")
  (if (eq 1 arg)
      (ido-dired)
    (dired default-directory)))

;;; DISK
(use-package disk
  :commands disk
  :ensure disk)

;;; EXPAND-REGION
(use-package expand-region
  :ensure expand-region)

;;; FILLADAPT -- WTF isn't this part of emacs by default by now?!
(use-package filladapt
  :ensure genehack-misc-elisp)

;;; FIXME
(use-package fixme
  :config (add-to-list 'fixme-modes 'go-mode)
  :ensure genehack-misc-elisp)

;;; FLYCHECK
;;;; https://github.com/flycheck/flycheck
(use-package flycheck-color-mode-line
  :commands flycheck-color-mode-line-mode
  :ensure flycheck-color-mode-line)

(use-package flycheck
  :commands flycheck-define-checker global-flycheck-mode
  :config (progn
            (add-hook 'after-init-hook #'global-flycheck-mode)
            (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)
            (defun genehack/include-perl-lib-p ()
              "Add 'lib' subdir to '-I' option of flycheck cmd if it exists."
              (defvar project/lib "")
              (if (projectile-project-p)
                  (let ((root (projectile-project-root)))
                    (setq project/lib (concat root "lib"))
                    (if (and (file-exists-p project/lib)
                             (file-directory-p project/lib))
                        (concat "-I" project/lib)))))
            (setq flycheck-perlcritic-severity "5")
            (add-to-list 'flycheck-checkers 'genehack/perl-perlcritic)
            (add-to-list 'flycheck-checkers 'perl-with-lib-from-project-root))
  :defer t
  :ensure flycheck)

(flycheck-define-checker perl-with-lib-from-project-root
                         "A Perl syntax checker using the Perl interpreter.

Uses projectile to find the project root, and if there is a 'lib'
directory there, adds it to PERL5LIB.

See URL `http://www.perl.org'."
                         :command ("perl" "-w" "-c" (eval (genehack/include-perl-lib-p)) source)
                         :error-patterns
                         ((error line-start (minimal-match (message))
                                 " at " (file-name) " line " line
                                 (or "." (and ", " (zero-or-more not-newline))) line-end))
                         :modes (perl-mode cperl-mode)
                         :next-checkers (genehack/perl-perlcritic))

(flycheck-define-checker genehack/perl-perlcritic
                         "A Perl syntax checker using Perl::Critic.

See URL `http://search.cpan.org/~thaljef/Perl-Critic/'.

Modified to use original source file so that
RequireFilenameMatchPackage policy works properly.
"
                         :command ("perlcritic" "--no-color" "--verbose" "%f:%l:%c:%s:%m (%e)\n"
                                   (option "--severity" flycheck-perlcritic-severity)
                                   source-original)
                         :error-patterns
                         ((info line-start
                                (file-name) ":" line ":" column ":" (any "1") ":" (message)
                                line-end)
                          (warning line-start
                                   (file-name) ":" line ":" column ":" (any "234") ":" (message)
                                   line-end)
                          (error line-start
                                 (file-name) ":" line ":" column ":" (any "5") ":" (message)
                                 line-end))
                         :modes (cperl-mode perl-mode)
                         :predicate (lambda () (and (buffer-file-name)
                                                    (not (buffer-modified-p)))))

;;; GIT BLAME FOR LINE
(defun genehack/git-blame-for-line ()
  "Show git blame for current line."
  (interactive)
  (defvar blame-out "")
  (let ((blame-line (line-number-at-pos (point)))
        (blame-file (buffer-file-name)))
    (setq blame-out (shell-command-to-string
                     (format "~/bin/git-blame-from-line-num %s %s" blame-line blame-file)))
    (with-output-to-temp-buffer "*git blame*" (princ blame-out))))

;;; GIT COMMIT MODE
(use-package git-commit
  :commands git-commit
  :ensure git-commit)

;;; GITHUB-BROWSE-FILE
(use-package github-browse-file
  :commands github-browse-file
  :ensure github-browse-file)

;;; GO
(use-package go-mode
  :commands go-mode
  :config (progn
            (setq gofmt-command "goimports")
            (add-hook 'before-save-hook 'gofmt-before-save)
            (add-hook 'go-mode-hook
                      (lambda ()
                        (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
                        (local-set-key (kbd "C-c i")   'go-goto-imports)
                        (local-set-key (kbd "M-.")     'godef-jump))))
  :ensure go-mode)
;;;; depends on go-mode, so put this down here...
(use-package company-go
  :ensure company-go)
(use-package go-snippets
  :disabled t
  :ensure go-snippets
  :init (go-snippets-initialize))

;;; HTML TIDY
(use-package tidy
  :commands tidy-buffer tidy-parse-config-file tidy-save-settings tidy-build-menu
  :ensure tidy)

(defun genehack/scrub-win-to-html ()
  "Scrub dumb quotes and other common Latin-1 stuff into HTML entities."
  (interactive)
  (save-excursion
    (dolist (thing '(("’"  . "&#8127;")
                     ("–" . "-" )
                     ("—" . "&#8212;")
                     ("“"  . "&#8220;")
                     ("”"  . "&#8221;")
                     ("™" . "&#8482;")
                     ("…" . "&#8230;")
                     ))
      (let ((match (car thing))
            (replace (cdr thing)))
        (goto-char (point-min))
        (while (re-search-forward match nil t)
          (replace-match replace nil nil))))))

;;; IDO
(use-package flx-ido
  :ensure flx-ido)
(use-package ido
  :init (progn
          (ido-mode t)
          (ido-everywhere)
          (flx-ido-mode t))
  :config (progn
            (setq ido-enable-flex-matching t)
            (setq ido-use-faces t)
            (setq ido-use-filename-at-point t))
  :ensure ido)

;;; JS2
(use-package js2-mode
  :commands js2-mode
  :config (progn
            (add-hook 'js2-init-hook 'genehack/js2-mode-setup)
            (setq-default js2-basic-offset 2))
  :ensure js2-mode
  :mode "\\.js$")

(defvar genehack/js2-keybindings-to-remove
  '(
    "\C-c\C-a"
    ) "List of keybindings to unset in 'js2-mode' buffers.
since 'js2-mode' steps on bindings I use globally..." )

(defun genehack/js2-mode-setup ()
  "Set up my js2-mode buffers."
  (dolist (binding genehack/js2-keybindings-to-remove)
    (local-unset-key (edmacro-parse-keys binding))))

;;; KILL THIS BUFFER
(defun genehack/kill-this-buffer ()
  "Kill the current buffer without prompting."
  (interactive)
  (kill-buffer nil))

;;; KOLON-MODE
(use-package kolon-mode
  :commands kolon-mode
  :ensure kolon-mode)

;;; LINE NUMBERS WITH M-G
;; from http://whattheemacsd.com//key-bindings.el-01.html
(global-set-key [remap goto-line] 'goto-line-with-feedback)

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input."
  (interactive)
  (defvar goto/line 0)
  (unwind-protect
      (progn
        (linum-mode 1)
        (setq goto/line (read-number "Goto line: "))
        (goto-char (point-min))
        (forward-line (1- goto/line)))
    (linum-mode -1)))

;;; MACRO
(use-package macro
  :commands macro-clear macro-dwim
  :ensure genehack-misc-elisp)

;;; MAGIT
(defvar genehack/git-executable (executable-find "git")
  "Path to active git executable.")

(use-package magit
  :if genehack/git-executable
  :commands magit-status
  :config (progn
            (define-key magit-status-mode-map (kbd "q") 'magit-quit-session))
  :ensure magit)

(if genehack/git-executable
    (defun genehack/magit-status-with-prompt (dir)
      "Prompt for git repo path then call magit-status on it."
      (interactive "Dgit repo: ")
      (magit-status-internal dir))
  (defun genehack/magit-status-with-prompt ()
    "Stub function for when git isn't available"
    (interactive)
    (message "Unable to find a git binary; magit is unavailable.")))

;;; http://endlessparentheses.com/automatically-configure-magit-to-access-github-prs.html
(defun genehack/add-pr-fetch ()
  "If refs/pull is not defined on a GH repo, define it."
  (let ((fetch-address "+refs/pull/*/head:refs/pull/origin/*"))
    (unless (member fetch-address (magit-get-all "remote" "origin" "fetch"))
      (when (string-match "github" (magit-get "remote" "origin" "url"))
        (magit-git-string "config" "--add" "remote.origin.fetch" fetch-address)))))
(add-hook 'magit-mode-hook #'genehack/add-pr-fetch)

(defun genehack/magit-key (arg)
  "Call magit-status, or with ARG call genehack/magit-status-with-prompt."
  (interactive "P")
  (if arg
      (call-interactively 'genehack/magit-status-with-prompt)
    (call-interactively 'magit-status)))

;;;; from http://whattheemacsd.com/setup-magit.el-01.html
(defadvice magit-status (around magit-fullscreen activate)
  "Full screen 'magit-status'."
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restore the previous window configuration and kill the magit buffer."
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

;;; MARKDOWN
(use-package markdown-mode
  :commands markdown-mode
  :ensure markdown-mode
  :mode "\\.mr?kd")

;;; MOVE TEXT
(use-package move-text
  :ensure move-text
  :init (move-text-default-bindings))

;;; MULTI-TERM
(setq system-uses-terminfo nil)
(use-package multi-term
  :config (progn
            (defalias 'term 'multi-term)
            (custom-set-variables
             '(term-default-bg-color "#000000")
             '(term-default-fg-color "#cccccc"))
            (setq multi-term-dedicated-select-after-open-p t
                  multi-term-dedicated-window-height 24))
  :ensure multi-term)

;;; NOOP
(defun genehack/noop nil "..." (interactive))

;;; OPEN LINE
;;;; from http://whattheemacsd.com//editing-defuns.el-01.html
(defun open-line-below ()
  "Open a line below the current line."
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  "Open a line above the current line."
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

;;; OPEN WITH
;;;; from http://emacsredux.com/blog/2013/03/27/open-file-in-external-program/
(defun open-with ()
  "Open the underlying file of a buffer in an external program."
  (interactive)
  (when buffer-file-name
    (shell-command (concat
                    (if (eq system-type 'darwin)
                        "open"
                      (read-shell-command "Open current file with: "))
                    " "
                    buffer-file-name))))

;;; PAREN-BOUNCE
;;;; ganked from <http://elfs.livejournal.com/1216037.html>
(defun genehack/paren-bounce ()
  "Bounce from one paren to the matching paren."
  (interactive)
  (let ((prev-char (char-to-string (preceding-char)))
        (next-char (char-to-string (following-char))))
    (cond ((string-match "[[{(<]" next-char) (forward-sexp 1))
          ((string-match "[\]})>]" prev-char) (backward-sexp 1))
          (t (error "%s" "Not an expression boundary.")))))

;;; PROJECTILE
(use-package projectile
  :config (progn
            (setq projectile-cache-file ".projectile.cache")
            (setq projectile-globally-ignored-files '("TAGS" ".git"))
            (setq projectile-known-projects-file
                  (expand-file-name "projectile-bookmarks.eld" genehack/emacs-tmp-dir)))
  :diminish projectile-mode
  :ensure projectile
  :init (progn
          (eval-when-compile (defvar genehack/emacs-tmp-dir))
          (projectile-global-mode)))

(defun genehack/find-file ()
  "Switch between `projectile-file-file` and `ido-find-file` based on value of `projectile-project-p`."
  (interactive)
  (if (projectile-project-p)
      (projectile-find-file)
    (ido-find-file)))

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

;;; SCALA
(use-package ensime
  :if (file-exists-p "/opt/ensime")
  :config (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
  :init (add-to-list 'load-path "/opt/ensime/elisp"))

;;; SCRATCH-BUFFER
(defun genehack/create-scratch-buffer nil
  "(re)create a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (insert initial-scratch-message)
  (lisp-interaction-mode))

;;; SMART-TAB
(use-package smart-tab
  :config (progn
            (setq smart-tab-using-hippie-expand t)
            (setq smart-tab-completion-functions-alist
                  '((cperl-mode      . genehack/company-yasnippet-or-completion)
                    (emacs-lisp-mode . genehack/company-yasnippet-or-completion)
                    (lisp-mode       . genehack/company-yasnippet-or-completion)
                    (go-mode         . genehack/company-yasnippet-or-completion)
                    (text-mode       . dabbrev-completion)))
            (global-smart-tab-mode 1))
  :diminish smart-tab-mode
  :ensure smart-tab)

;;; SMARTPARENS
(use-package smartparens
  :ensure smartparens
  :config (progn
            ;; do not autoinsert ' pair if the point is preceeded by
            ;; word.  This will handle the situation when ' is used as
            ;; a contraction symbol in natural language.  Nil for
            ;; second argument means to keep the original definition
            ;; of closing pair.
            (sp-pair "'" nil :unless '(sp-point-after-word-p))
            ;; emacs is lisp hacking enviroment, so we set up some most common
            ;; lisp modes too
            (sp-with-modes sp--lisp-modes
              ;; disable ', it's the quote character!
              (sp-local-pair "'" nil :actions nil))
  :diminish smartparens-mode
  :init (progn
          (smartparens-global-mode 1)
          (show-smartparens-global-mode t))))

;;; SMEX
(use-package smex
  :ensure smex
  :init (smex-initialize))

;;; SPLIT-(HORIZONT|VERTIC)ALLY-OR-DELETE-OTHER-WINDOWS
(defun genehack/split-horizontally-or-delete-other-windows ()
  "If one window, split horizontally; otherwise, 'delete-other-windows'."
  (interactive)
  (if (eq 1 (count-windows nil))
      (split-window-horizontally)
    (delete-other-windows)))

(defun genehack/split-vertically-or-delete-other-windows ()
  "If one window, split vertically; otherwise, 'delete-other-windows'."
  (interactive)
  (if (eq 1 (count-windows nil))
      (split-window-vertically)
    (delete-other-windows)))

;;; STRIP TRAILING WHITESPACE
(setq-default show-trailing-whitespace t)
(defvar genehack/strip-trailing-whitespace-in-these-modes
  '(
    clojure-mode
    cperl-mode
    css-mode
    emacs-lisp-mode
    js2-mode
    lisp-mode
    org-mode
    ruby-mode
    scala-mode
    tt-mode
    yaml-mode
    web-mode
    )
  "List of modes where trailing whitespace should be stripped when saving files.")

;;;; inspired by http://whattheemacsd.com/buffer-defuns.el-01.html
(defun genehack/strip-whitespace ()
  "Untabify, strip white space, set file coding to UTF8."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

(defun genehack/strip-whitespace-and-indent ()
  "Strip various whitespaces and reindent whole file."
  (interactive)
  (genehack/strip-whitespace)
  (indent-region (point-min) (point-max)))

(defun genehack/set-up-whitespace-strip-in-these-modes ()
  "Set up whitespace stripping in the modes in genehack/strip-trailing-whitespace-in-these-modes."
  (if (member major-mode genehack/strip-trailing-whitespace-in-these-modes)
      (genehack/strip-whitespace)))
(add-hook 'before-save-hook 'genehack/set-up-whitespace-strip-in-these-modes)

(defun genehack/kill-space-forward ()
  "Delete white space to the right of point."
  (interactive)
  (save-excursion
    (let* ((here (point)))
      (skip-chars-forward " ")
      (delete-region here (point)))))

;;; TEMPLATE
(use-package template-mode
  :commands template-minor-mode
  :config (add-hook 'html-mode-hook 'genehack/enable-template-minor-mode)
  :ensure genehack-perl-elisp)

(defun genehack/enable-template-minor-mode ()
  "Turn on 'template-minor-mode' in *.tt files."
  (if (string-match "\\.tt2?$" buffer-file-name)
      (template-minor-mode 1)))

;;; TEXT-SCALE
(defun genehack/text-scale-default ()
  "Set text scale to default."
  (interactive)
  (text-scale-set 0))

;;; THEME
(defun genehack/solarize-this ()
  "Enable solarized theme."
  (interactive)
  (load-theme 'solarized-dark t))

(use-package solarized-theme
  :ensure solarized-theme
  :init (genehack/solarize-this))

(defun genehack/solarize-this-light ()
  "Enable solarized theme."
  (interactive)
  (load-theme 'solarized-light t))

(use-package twilight-theme
  :ensure twilight-theme)

(defun genehack/twilight-this ()
  "Enable twilight theme."
  (interactive)
  (load-theme 'twilight t))

;;; TOGGLE-BOL
(defun genehack/bol-toggle ()
  "Toggle between beginning of indent and beginning of line."
  (interactive)
  (let ((genehack/bol-command-name "genehack/bol-toggle"))
    (setq this-command genehack/bol-command-name)
    (cond ((equal (point) (point-at-bol)) (back-to-indentation))
          ((equal last-command genehack/bol-command-name) (move-beginning-of-line nil))
          (t (back-to-indentation)))))

;;; UNICODE
(defun genehack/unicode (char)
  "Insert Unicode character CHAR at point."
  (interactive "MCharacter name? ")
  (insert (shell-command-to-string (format "uni %s" char))))

;;; URL ENCODING
;;;; based on http://twitter.com/#!/OvidPerl/status/28076709865586688
(defun genehack/unescape_uri (begin end)
  "URI unescape region between BEGIN and END."
  (interactive "r")
  (shell-command-on-region
   begin end
   "perl -MURI::Escape -e 'print URI::Escape::uri_unescape(do { local $/; <STDIN> })'"
   'current-buffer t))

;;; WEB-MODE
(use-package web-mode
  :config (progn
            (add-to-list 'safe-local-variable-values
                         '(web-mode-markup-indent-offset . 4))
            (add-hook 'web-mode-hook 'genehack/web-mode-setup))
  :ensure web-mode
  :mode "\\.\\(html\\|tx\\)")

(defvar genehack/web-mode-keybindings-to-remove
  '(
    "\C-c\C-a"
    ) "List of keybindings to unset in 'web-mode' buffers.
since 'web-mode' steps on bindings I use globally..." )

(defun genehack/web-mode-setup ()
  "Set up my web-mode buffers."
  (dolist (binding genehack/web-keybindings-to-remove)
    (local-unset-key (edmacro-parse-keys binding))))

;;; YAML-MODE
(use-package yaml-mode
  :commands yaml-mode
  :ensure yaml-mode
  :mode "\\.ya?ml$")

;;; YASNIPPET
(eval-when-compile (defvar genehack/emacs-dir))

(defvar genehack/yas-snippet-dir (concat genehack/emacs-dir "share/snippets")
  "Directory with snippets.")

(use-package yasnippet
  :config (progn
            (yas-load-directory genehack/yas-snippet-dir)
            (setq yas-snippet-dirs (delete "~/.emacs.d/snippets" yas-snippet-dirs))
            (add-to-list 'yas-snippet-dirs genehack/yas-snippet-dir)
            (yas-global-mode))
  :diminish yas-minor-mode
  :ensure yasnippet
  :init (progn
          (setq yas-use-menu nil)))

(if (file-exists-p genehack/yas-snippet-dir)
    (unless (file-directory-p genehack/yas-snippet-dir)
      (error "Snippets directory creation blocked by file"))
  (make-directory genehack/yas-snippet-dir))

;;;; update cursor color based on yasnippet status
;;;;; http://stackoverflow.com/questions/14264228/how-can-i-trigger-an-event-when-a-yasnippet-macro-can-fire
;;;;; https://gist.github.com/4506396
(defvar genehack/default-cursor-color "red")
(defvar genehack/default-cursor-type 'box)
(defvar genehack/yasnippet-can-fire-cursor-color "green")
(defvar genehack/yasnippet-can-fire-cursor-type 'box)

;;;;; It will test whether it can expand, if yes, cursor color -> green.
(defun genehack/yasnippet-can-fire-p (&optional field)
  "Predicate for whether a yasnippet can fire.
Not sure what FIELD is for ..."
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
                  (yas--templates-for-key-at-point))
              (yas--templates-for-key-at-point))))
    (and templates-and-pos (first templates-and-pos))))

(defun yasnippet-change-cursor-color-when-can-fire (&optional field)
  "Change cursor color when a yasnippet could fire.
Again, not sure what FIELD does..."
  (interactive)
  (if (genehack/yasnippet-can-fire-p)
      (progn
        (set-cursor-color genehack/yasnippet-can-fire-cursor-color)
        (setq cursor-type genehack/yasnippet-can-fire-cursor-type))
    (set-cursor-color genehack/default-cursor-color)
    (setq cursor-type genehack/default-cursor-type)))

;;;;; As pointed out by Dmitri, this will make sure it will update color when needed.
(add-hook 'post-command-hook 'yasnippet-change-cursor-color-when-can-fire)

(provide 'misc)
;;; misc.el ends here
