;; misc.el --- various customizations and additions
;;; Commentary:
;;; This is for stuff that _isn't_ built-in to Emacs

;;; Code:

(require 'use-package)

;;; PROJECTILE
;;;; this is at the top because things below require it.
(use-package projectile
  :ensure t
  :defer t
  :diminish (projectile-mode . " pj")
  :config
  (setq projectile-cache-file ".projectile.cache")
  (setq projectile-completion-system 'ivy)
  (setq projectile-globally-ignored-files '("TAGS" ".git"))
  (setq projectile-known-projects-file
        (expand-file-name "projectile-bookmarks.eld" genehack/emacs-tmp-dir))
  (setq projectile-switch-project-action 'projectile-dired)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (add-hook 'projectile-after-switch-project-hook 'genehack/node-project-setup)
  (projectile-cleanup-known-projects)
  (defun genehack/find-file (arg)
    "Pick `projectile-file-file` or `counsel-find-file` (Force latter w/ARG).
Decision is based on the value of `projectile-project-p`.  If
given a prefix arg ARG, unconditionally use `counsel-find-file`."
    (interactive "P")
    (if (and (projectile-project-p) (null arg))
        (projectile-find-file)
      (counsel-find-file)))
  :defines genehack/emacs-tmp-dir
  :functions
  projectile-project-p
  :init
  (projectile-mode +1))

;;; AG
(use-package ag
  :commands ag
  :ensure t
  :defer t
  :defines ag-highlight-search
  :config
  (setq ag-highlight-search t))

(defun genehack/kill-ag-buffers ()
  "Kill all buffers that start with '*ag search text:.'."
  (interactive)
  (dolist (buffer (buffer-list))
    (if (string-match "^*ag search text:" (buffer-name buffer))
        (kill-buffer buffer))))

;;; AGGRESSIVE INDENT MODE
(use-package aggressive-indent
  :ensure t
  :defer t
  :diminish (aggressive-indent-mode " ag")
  :init
  (global-aggressive-indent-mode 1)
  (defvar aggressive-indent-excluded-modes)
  (add-to-list 'aggressive-indent-excluded-modes
               'html-mode
               'web-mode))

;;; API BLUEPRINT
(use-package apib-mode
  :ensure t
  :defer t
  :mode "\\.apib\\'")

;;; AUTO CREATE DIRECTORIES
;;;; after <http://atomized.org/2008/12/emacs-create-directory-before-saving/>
(defun genehack/set-up-before-save-hook ()
  "My customizations for 'before-save-hook'."
  (or (file-exists-p (file-name-directory buffer-file-name))
      (make-directory (file-name-directory buffer-file-name) t)))
(add-hook 'before-save-hook 'genehack/set-up-before-save-hook)

;;; BROWSE-KILL-RING
(use-package browse-kill-ring
  :ensure t
  :defer t
  :init
  (browse-kill-ring-default-keybindings))


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

;;; COUNSEL (also IVY and SWIPER)
(use-package all-the-icons-ivy
  :ensure t
  :defer t)
(use-package counsel
  :ensure t
  :defer t
  :diminish (ivy-mode . " ivy")
  :after all-the-icons-ivy
  :config
  (all-the-icons-ivy-setup)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-re-builders-alist '((counsel-M-x . ivy--regex-fuzzy) ; Only counsel-M-x use flx fuzzy search
                                (t . ivy--regex-plus)))
  :init (ivy-mode 1))
(setq font-lock-global-modes '(not dired-mode))


;;; CSS-HEXCOLOR
(use-package css-hexcolor
  :ensure genehack-misc-elisp
  :defer t)

;;; DIFF-CURRENT-BUFFER-WITH-FILE
(defun genehack/diff-current-buffer-with-file ()
  "Show diff between current buffer contents and file on disk."
  (interactive)
  (diff-buffer-with-file (current-buffer)))

;;; DIMINISH
;;;; from http://whattheemacsd.com/init.el-04.html
(use-package diminish
  :ensure t
  :defer t)

;;; DIR-LOCALS-UPWARD
;;; from https://emacs.stackexchange.com/questions/5527/is-there-a-way-to-daisy-chain-dir-locals-el-files
(defvar walk-dir-locals-upward nil
  "If non-nil, eval .dir-locals.el files from current directory up the tree.
Otherwise they will be evaluated from the top down to the current directory.  Setting this to nil allows subdirectories to overload parent directory settings.")

(require 'subr-x) ;; for `string-join' in the defun below
(defun genehack/pop-dir (dir)
  "Pops a trailing directory off DIR to return the remainder of the path."
  (concat "/"
          ;; there's probably some better way to do this but ¯\_(ツ)_/¯
          (string-join (reverse (cdr (reverse (split-string dir "/" t)))) "/")
          "/"))

(defadvice hack-dir-local-variables (around walk-dir-locals-file activate)
  "Walk directory tree and load _all_ the .dir-locals.el files.
In a projectile project, walk the tree from the current directory up to one directory above the project root, adding any .dir-locals.el files that are found, skipping over empty directories. (It goes one level higher than the root to allow for applying common settings across  a group of related Git repos, e.g., work projects.) When not in a projectile project, walk the tree from the current directory upwards, adding any .dir-locals.el files that are found, but stopping once a directory without a .dir-locals.el file is found."
  (let* ((dir-locals-list (list))
         (walk-dir-locals-file dir-locals-file)
         (debug-dir-locals-walk nil))
    (if debug-dir-locals-walk (message "initial walk-dir-locals-file: %s" walk-dir-locals-file))
    (if (file-readable-p walk-dir-locals-file)
        (add-to-list 'dir-locals-list (expand-file-name walk-dir-locals-file) walk-dir-locals-upward))
    (if (projectile-project-p)
        (let ((root (concat (projectile-parent (projectile-project-root)) "/"))
              (dir (expand-file-name default-directory)))
          (while (not (equal root dir))
            (progn
              ;; TODO I'm not convinced this is actually doing the
              ;; right thing but it seems to be working so...
              (if debug-dir-locals-walk (message "root: %s -> dir :%s" root dir))
              (setq walk-dir-locals-file (concat "../" walk-dir-locals-file))
              (if debug-dir-locals-walk (message "walk dir locals file: %s" walk-dir-locals-file))
              (if (file-readable-p walk-dir-locals-file)
                  (add-to-list 'dir-locals-list (expand-file-name walk-dir-locals-file) walk-dir-locals-upward))
              (setq dir (genehack/pop-dir dir)))))
      (while (file-readable-p (concat "../" walk-dir-locals-file))
        (progn
          (setq walk-dir-locals-file (expand-file-name (concat "../" walk-dir-locals-file)))
          (add-to-list 'dir-locals-list walk-dir-locals-file walk-dir-locals-upward))))
    (if debug-dir-locals-walk (message "List: %S" dir-locals-list))
    (dolist (file dir-locals-list)
      (message "Loading file: %s" file)
      (let ((dir-locals-file (expand-file-name file)))
        ad-do-it))))

;;; DISK
(use-package disk
  :ensure t
  :defer t
  :commands disk)

;;; DUMB-JUMP
(use-package dumb-jump
  :disabled t
  :ensure t
  :defer t
  :diminish (dumb-jump-mode . " dj")
  :init (dumb-jump-mode))

;;; EXPAND-REGION
(use-package expand-region
  :ensure t
  :defer t)

;;; FILLADAPT -- WTF isn't this part of emacs by default by now?!
(use-package filladapt
  :ensure genehack-misc-elisp
  :defer t
  :diminish (filladapt-mode . " fa"))

;;; FIXME
(use-package fixme
  :ensure genehack-misc-elisp
  :defer t
  :config
  (add-to-list 'fixme-modes 'go-mode)
  (add-to-list 'fixme-modes 'js-mode)
  (add-to-list 'fixme-modes 'js2-mode))

;;; FLYCHECK
;;;; https://github.com/flycheck/flycheck
(use-package flycheck-color-mode-line
  :ensure t
  :defer t
  :commands flycheck-color-mode-line-mode)

(use-package flycheck
  :ensure t
  :defer t
  :diminish (flycheck-mode . " FC")
  :commands flycheck-define-checker global-flycheck-mode
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)
  (defun genehack/include-perl-lib-p (lib)
    "Add 'lib' subdir to '-I' option of flycheck cmd if it exists."
    (defvar project/lib "")
    (if (projectile-project-p)
        (let ((root (projectile-project-root)))
          (setq project/lib (concat root lib))
          (if (and (file-exists-p project/lib)
                   (file-directory-p project/lib))
              (concat "-I" project/lib)))))
  (setq flycheck-perlcritic-severity "5")
  (add-to-list 'flycheck-checkers 'genehack/perl-perlcritic)
  (add-to-list 'flycheck-checkers 'perl-with-lib-from-project-root)
  :functions
  projectile-project-p
  projectile-project-root)

(flycheck-define-checker perl-with-lib-from-project-root
  "A Perl syntax checker using the Perl interpreter.

Uses projectile to find the project root, and if there are 'lib'
or 'local/lib/perl5' directories there, adds them to PERL5LIB."
  :command
  ("perl" "-w" "-c"
   (eval (genehack/include-perl-lib-p "lib"))
   (eval (genehack/include-perl-lib-p "local/lib/perl5"))
   source)
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
RequireFilenameMatchPackage policy works properly."
  :command
  ("perlcritic" "--no-color" "--verbose" "%f:%l:%c:%s:%m (%e)\n"
   (config-file "--profile" flycheck-perlcriticrc)
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
  :ensure t
  :defer t
  :commands git-commit)

;;; GIT-GUTTER
(use-package git-gutter
  :ensure t
  :defer t
  :diminish (git-gutter-mode . " gg")
  :config
  (global-git-gutter-mode t))

;;; GITHUB-BROWSE-FILE
(use-package github-browse-file
  :ensure t
  :defer t
  :commands github-browse-file)

;;; GO
(use-package go-mode
  :ensure t
  :defer t
  :commands go-mode
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook (lambda () (local-set-key (kbd "M-.") 'godef-jump)))  )

;;; HTML TIDY
(use-package tidy
  :ensure t
  :defer t
  :commands
  tidy-buffer
  tidy-parse-config-file
  tidy-save-settings
  tidy-build-menu)

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

;;; JSON
(use-package json-mode
  :ensure t
  :defer t
  :commands json-mode
  :config
  (add-hook 'json-mode-hook #'prettier-js-mode)
  (add-to-list 'safe-local-variable-values '(json-mode-indent-level . 2))
  (add-to-list 'safe-local-variable-values '(json-mode-indent-level . 4))
  (setq-default json-mode-indent-level 2)
  :mode "\\.json\\'")

;;; JS2
(use-package eslint-fix
  :ensure t
  :defer t)
(use-package prettier-js
  :ensure t
  :defer t
  :diminish (prettier-js-mode . " pjs"))
(use-package add-node-modules-path
  :ensure t
  :defer t)
(use-package js2-mode
  :ensure t
  :defer t
  :diminish (js2-mode . " js2")
  :commands js2-mode
  :config
  (add-hook 'js2-mode-hook #'add-node-modules-path)
  (add-hook 'js2-mode-hook #'prettier-js-mode)
  (add-hook 'js2-mode-hook (lambda() (add-hook 'after-save-hook 'eslint-fix nil t)))
  (add-hook 'js2-init-hook #'genehack/js2-mode-setup)
  (add-to-list 'safe-local-variable-values '(js2-basic-offset . 2))
  (add-to-list 'safe-local-variable-values '(js2-basic-offset . 4))
  (setq-default js2-basic-offset 2)
  :mode "\\.js\\'")

(defvar genehack/js2-keybindings-to-remove
  '(
    "\C-c\C-a"
    ) "List of keybindings to unset in 'js2-mode' buffers.
since 'js2-mode' steps on bindings I use globally..." )

;;; based on docs on https://github.com/emacs-lsp/lsp-javascript
(defun genehack/company-transformer (candidates)
  "My company transformer of CANDIDATES."
  (let ((completion-ignore-case t))
    (all-completions (company-grab-symbol) candidates)))

;;; nvm stuff
(defvar genehack/node-version "" "Version of Node to use as read from .nvmrc file.")
(defvar genehack/nvmrc-file ".nvmrc" "Path to nvmrc file relative to project root.")
(defun genehack/node-project-setup ()
  "Use nvm to set active Node version if .nvmrc file exists in project root."
  (interactive)
  (if (file-exists-p genehack/nvmrc-file)
      (progn
        (setq genehack/node-version (chomp-end
                                     (with-temp-buffer
                                       (insert-file-contents genehack/nvmrc-file)
                                       (buffer-string))))
        (genehack/nvm genehack/node-version)
        (message "Set up to use node version %s" genehack/node-version))))

(require 'company)
(defun genehack/js2-mode-setup ()
  "Set up my js2-mode buffers."
  ;; (setq company-backends '(company-ycmd))
  (make-local-variable 'company-transformers)
  (push 'genehack/company-transformer company-transformers)
  (add-hook 'js2-mode-hook #'lsp-javascript-typescript-enable)
  (dolist (binding genehack/js2-keybindings-to-remove)
    (local-unset-key (edmacro-parse-keys binding))))

;;; KILL THIS BUFFER
(defun genehack/kill-this-buffer ()
  "Kill the current buffer without prompting."
  (interactive)
  (kill-buffer nil))

;;; KOLON-MODE
(use-package kolon-mode
  :ensure t
  :defer t
  :commands kolon-mode)

;;; LINE NUMBERS WITH M-G
;; from http://whattheemacsd.com//key-bindings.el-01.html
(global-set-key [remap goto-line] 'goto-line-with-feedback)

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input."
  (interactive)
  (defvar goto/line 0)
  (unwind-protect
      (progn
        ;; display-line-numbers is Emacs 26 specific; if this gives
        ;; you problems, conditionalize between it and linum-mode
        ;; depending on Emacs version
        (display-line-numbers-mode 1)
        (setq goto/line (read-number "Goto line: "))
        (goto-char (point-min))
        (forward-line (1- goto/line)))
    (display-line-numbers-mode -1)))

;;; MACRO
(use-package macro
  :ensure genehack-misc-elisp
  :defer t
  :commands macro-clear macro-dwim)

;;; MAGIT
(defvar genehack/git-executable (executable-find "git") "Path to active git executable.")

(use-package magit
  :ensure t
  :defer t
  :if genehack/git-executable
  :commands magit-status
  :config
  (defvar magit-push-always-verify)
  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)
  (setq magit-completing-read-function 'ivy-completing-read)
  (setq magit-push-always-verify nil)
;;; modified from http://endlessparentheses.com/automatically-configure-magit-to-access-github-prs.html
  (defun genehack/add-pr-fetch ()
    "If refs/pull is not defined on a GH repo, define it."
    (let ((fetch-address "+refs/pull/*/head:refs/pull/origin/*"))
      (unless (member fetch-address (magit-get-all "remote" "origin" "fetch"))
        (let ((repo-remote (magit-get "remote" "origin" "url")))
          (unless (eq repo-remote nil)
            (when (string-match "github" repo-remote)
              (magit-git-string "config" "--add" "remote.origin.fetch" fetch-address)))))))
  (add-hook 'magit-mode-hook #'genehack/add-pr-fetch)

  :functions
  genehack/add-pr-fetch
  magit-get
  magit-get-all
  magit-git-string)

(if genehack/git-executable
    (defun genehack/magit-status-with-prompt (dir)
      "Prompt for git repo path then call magit-status on it."
      (interactive "Dgit repo: ")
      (magit-status-internal dir))
  (defun genehack/magit-status-with-prompt ()
    "Stub function for when git isn't available"
    (interactive)
    (message "Unable to find a git binary; magit is unavailable.")))

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
  :ensure t
  :defer t
  :commands markdown-mode
  :mode "\\.mr?kd"
  :config
  (setq markdown-command "Markdown.pl"))

;;; MOVE TEXT
(use-package move-text
  :ensure t
  :defer t
  :init
  (move-text-default-bindings))

;;; MULTI-TERM
(setq system-uses-terminfo nil)
(use-package multi-term
  :ensure t
  :defer t
  :config
  (defalias 'term 'multi-term)
  (custom-set-variables
   '(term-default-bg-color "#000000")
   '(term-default-fg-color "#cccccc"))
  (setq multi-term-dedicated-select-after-open-p t
        multi-term-dedicated-window-height 24))

;;; NODEJS-REPL
(use-package nodejs-repl
  :ensure t
  :defer t
  :commands nodejs-repl)

;;; NOOP
(defun genehack/noop nil "..." (interactive))

;;; NVM
(use-package nvm
  :ensure t
  :defer t
  :config
  ;; this bit depends on pulling this in from exec-shell,
  ;; which is done in init.el.
  (setq nvm-dir (getenv "NVM_DIR")))

(defun genehack/nvm (version)
  "Reconfigure $PATH and `exec-path' to use a particular Node VERSION via nvm."
  (interactive "sVersion: ")
  (nvm-use version)
  (setq exec-path (parse-colon-path (getenv "PATH"))))

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
;;;; originally ganked from <http://elfs.livejournal.com/1216037.html>
(defun genehack/paren-bounce ()
  "Bounce from one paren to the matching paren."
  (interactive)
  (let ((prev-char (char-to-string (preceding-char)))
        (next-char (char-to-string (following-char))))
    (cond ((string-match "[[{(<\"']" next-char) (forward-sexp 1))
          ((string-match "[\]})>\"']" prev-char) (backward-sexp 1))
          (t (error "%s" "Not an expression boundary.")))))



;; from https://www.emacswiki.org/emacs/ElispCookbook#toc6
(defun chomp-end (str)
  "Chomp tailing whitespace from STR."
  (replace-regexp-in-string
   (rx (* (any " \t\n")) eos)
   ""
   str))

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
  :defer t
  :if
  (file-exists-p "/opt/ensime")
  :config
  (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
  :load-path "/opt/ensime/elisp")

;;; SCRATCH-BUFFER
(defun genehack/create-scratch-buffer nil
  "(re)create a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (insert initial-scratch-message)
  (lisp-interaction-mode))

;;; SMARTPARENS
(use-package smartparens
  :ensure t
  :defer t
  :diminish (smartparens-mode . " SP")
  :init
  (require 'smartparens-config)
  (smartparens-global-mode 1)
  (show-smartparens-global-mode t))

;;; SMEX
(use-package smex
  :ensure t
  :defer t
  :config
  (setq smex-save-file (concat genehack/emacs-tmp-dir "smex-items"))
  :init
  (smex-initialize))

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
    c++-mode
    clojure-mode
    cperl-mode
    csharp-mode
    css-mode
    emacs-lisp-mode
    js-mode
    js2-mode
    lisp-mode
    markdown-mode
    org-mode
    ruby-mode
    scala-mode
    swift-mode
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
  :defer t
  :config
  (add-hook 'html-mode-hook 'genehack/enable-template-minor-mode)
  :ensure genehack-perl-elisp)

(defun genehack/enable-template-minor-mode ()
  "Turn on 'template-minor-mode' in *.tt files."
  (if (string-match "\\.tt2?\\'" buffer-file-name)
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
  :defer t
  :init
  (genehack/solarize-this))

(defun genehack/solarize-this-light ()
  "Enable solarized theme."
  (interactive)
  (load-theme 'solarized-light t))

(use-package twilight-theme
  :ensure twilight-theme
  :defer t)

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

;;; UUID
(defun genehack/uuid ()
  "Insert a newly generated UUID at point.
Also copies the new UUID to the clipboard."
  (interactive)
  (let ((uuid (downcase
               (replace-regexp-in-string
                (rx (* (any " \t\n")) eos) ""
                (shell-command-to-string "uuidgen")))))
    (kill-new uuid)
    (insert uuid)))

;;; WEB-BEAUTIFY
(use-package web-beautify
  :ensure t
  :defer t
  :config
  (defvar json-mode-map)
  (defvar web-mode-map)
  (defvar css-mode-map)
  (eval-after-load 'js2-mode  '(define-key js2-mode-map  (kbd "C-c b") 'web-beautify-js))
  (eval-after-load 'json-mode '(define-key json-mode-map (kbd "C-c b") 'web-beautify-js))
  (eval-after-load 'sgml-mode '(define-key html-mode-map (kbd "C-c b") 'web-beautify-html))
  (eval-after-load 'web-mode  '(define-key web-mode-map  (kbd "C-c b") 'web-beautify-html))
  (eval-after-load 'css-mode  '(define-key css-mode-map  (kbd "C-c b") 'web-beautify-css)))

;;; WEB-MODE
(use-package web-mode
  :ensure t
  :defer t
  :config
  (add-to-list 'safe-local-variable-values
               '(web-mode-markup-indent-offset . 2))
  (add-hook 'web-mode-hook 'genehack/web-mode-setup)
  :mode "\\.\\(html\\|tx\\)")

(defvar genehack/web-mode-keybindings-to-remove
  '(
    "\C-c\C-a"
    ) "List of keybindings to unset in 'web-mode' buffers.
since 'web-mode' steps on bindings I use globally..." )

(defun genehack/web-mode-setup ()
  "Set up my web-mode buffers."
  (dolist (binding genehack/web-mode-keybindings-to-remove)
    (local-unset-key (edmacro-parse-keys binding))))

;;; YAML-MODE
(use-package yaml-mode
  :ensure t
  :defer t
  :commands yaml-mode
  :config
  (add-hook 'yaml-mode-hook #'prettier-js-mode)
  (add-to-list 'safe-local-variable-values '(yaml-indent-offset . 4))
  (define-key yaml-mode-map (kbd "RET") 'newline-and-indent)
  :mode "\\.ya?ml\\'")

;;; YASNIPPET
(eval-when-compile (defvar genehack/emacs-dir))
(defvar genehack/yas-snippet-dir (concat genehack/emacs-dir "share/snippets")
  "Directory with snippets.")

(use-package yasnippet
  :ensure t
  :defer t
  :diminish (yas-minor-mode . " yas")
  :functions
  genehack/yasnippet-can-fire-p
  yas-expand
  yas-load-directory
  yas--field-end
  yas--field-start
  yas--templates-for-key-at-point
  :config
  (yas-load-directory genehack/yas-snippet-dir)
  (setq yas-snippet-dirs (delete "~/.emacs.d/snippets" yas-snippet-dirs))
  (add-to-list 'yas-snippet-dirs genehack/yas-snippet-dir)
  (setq yas-use-menu nil)
  (yas-global-mode)

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
  (add-hook 'post-command-hook 'yasnippet-change-cursor-color-when-can-fire))

(provide 'misc)
;;; misc.el ends here
