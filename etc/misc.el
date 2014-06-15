;; misc.el --- various customizations and additions
;;; Commentary:
;;; This is for stuff that _isn't_ built-in to Emacs

;;; Code:

;;; AG
(require 'ag)
(setq ag-highlight-search t)

;;; ANNOYING ARROWS
(load "annoying-arrows-mode") ;; not require because linter bitches.
(global-annoying-arrows-mode)

;;; AUTO CREATE DIRECTORIES
;;;; after <http://atomized.org/2008/12/emacs-create-directory-before-saving/>
(defun genehack/set-up-before-save-hook ()
  "My customizations for 'before-save-hook'."
  (or (file-exists-p (file-name-directory buffer-file-name))
      (make-directory (file-name-directory buffer-file-name) t)))
(add-hook 'before-save-hook 'genehack/set-up-before-save-hook)

;;; BROWSE-KILL-RING
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;;; COMPANY-MODE
(require 'company)
(require 'company-go)
(global-company-mode)
(setq company-echo-delay 0
      company-idle-delay 0.3
      company-minimum-prefix-length 1)
(setq-default company-backends '((company-capf :with company-yasnippet)
                                 (company-dabbrev-code company-keywords)
                                 company-go
                                 company-nxml
                                 company-css
                                 company-files
                                 company-dabbrev))
;;;; inspired by https://gist.github.com/nonsequitur/265010
(require 'yasnippet)
(defun genehack/company-yasnippet-or-completion ()
  "Expand yasnippet if available, otherwise autocomplete."
  (interactive)
  (if (first (yas--current-key))
      (progn (company-abort)
             (yas-expand))
    (company-complete-common)))
(define-key company-active-map "\t" 'genehack/company-yasnippet-or-completion)

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
(require 'css-hexcolor)

;;; DIFF-CURRENT-BUFFER-WITH-FILE
(defun genehack/diff-current-buffer-with-file ()
  "Show diff between current buffer contents and file on disk."
  (interactive)
  (diff-buffer-with-file (current-buffer)))

;;; DIRED-RIGHT-HERE
(defun genehack/dired-right-here (arg)
  "Run 'ido-dired' or, with ARG, 'dired' on current active directory."
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

;;; FIPLR
(require 'fiplr)
(defun genehack/find-file ()
  "Switch between 'fiplr-find-find' and 'ido-find-file' depending on whether you're in a project or not."
  (interactive)
  (let ((cwd (if (buffer-file-name)
                 (directory-file-name (file-name-directory (buffer-file-name)))
               (file-truename "."))))
    (if (fiplr-find-root cwd fiplr-root-markers)
        (fiplr-find-file)
      (ido-find-file))))

;;; FIXME
(require 'fixme)

;;; FLYCHECK
;;;; https://github.com/flycheck/flycheck
(require 'flycheck)
(require 'flycheck-color-mode-line)
(require 'projectile)
(defun genehack/include-perl-lib-p ()
  "Add 'lib' subdir to '-I' option of flycheck cmd if it exists."
  (defvar project/lib "")
  (if (projectile-project-p)
      (let ((root (projectile-project-root)))
        (setq project/lib (concat root "lib"))
        (if (and (file-exists-p project/lib)
                 (file-directory-p project/lib))
            (concat "-I" project/lib)))))

(add-hook 'after-init-hook #'global-flycheck-mode)
(eval-after-load "flycheck" '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

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
            (option "--severity" flycheck-perlcritic-verbosity
                    flycheck-option-int)
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
(setq flycheck-perlcritic-verbosity "5")
(add-to-list 'flycheck-checkers 'genehack/perl-perlcritic)

;;; GIT BLAME FOR LINE
(defun genehack/git-blame-for-line ()
  "Show git blame for current line."
  (interactive)
  (defvar blame-out "")
  (let ((blame-line (line-number-at-pos (point)))
        (blame-file (buffer-file-name)))
    (setq blame-out (shell-command-to-string (format "~/bin/git-blame-from-line-num %s %s" blame-line blame-file)))
    (with-output-to-temp-buffer "*git blame*" (princ blame-out))))

;;; GIT - misc
(require 'git-commit-mode)

;;; GITHUB-BROWSE-FILE
(autoload 'github-browse-file "github-browse-file" "browse lines in current file on github")

;;; GO
(require 'go-mode-load)
(require 'go-snippets)
(go-snippets-initialize)
(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
            (local-set-key (kbd "C-c i")   'go-goto-imports)
            (local-set-key (kbd "M-.")     'godef-jump)))

;;; HTML TIDY
(autoload 'tidy-buffer             "tidy" "Run Tidy HTML parser on current buffer" t)
(autoload 'tidy-parse-config-file  "tidy" "Parse the `tidy-config-file'"           t)
(autoload 'tidy-save-settings      "tidy" "Save settings to `tidy-config-file'"    t)
(autoload 'tidy-build-menu         "tidy" "Install an options menu for HTML Tidy." t)

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
                     ))
      (let ((match (car thing))
            (replace (cdr thing)))
        (goto-char (point-min))
        (while (re-search-forward match nil t)
          (replace-match replace nil nil))))))

;;; IDO-UBI
;;;; from http://whattheemacsd.com//setup-ido.el-01.html
;;;; Use ido everywhere
(load "ido-ubiquitous") ;; not require because linter bitches.
(ido-ubiquitous-mode 1)

(defmacro ido-ubiquitous-use-new-completing-read (cmd package)
  "Fix 'ido-ubiquitous' for CMD in PACKAGE."
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
(autoload 'macro-dwim "macro" "DWIM macro recording and playback." t)
(autoload 'macro-clear "macro" "Clear last keyboard macro" t)

;;; MAGIT
(defvar genehack/git-executable (executable-find "git")
  "Path to active git executable.")

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

(require 'magit)
(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

;;; MARKDOWN
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.mr?kd" . markdown-mode))

;;; MOVE TEXT
(require 'move-text)
(move-text-default-bindings)

;;; MULTI-TERM
(setq system-uses-terminfo nil)
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
(eval-when-compile (defvar genehack/emacs-tmp-dir))
(require 'projectile)
(projectile-global-mode)
(setq projectile-cache-file ".projectile.cache")
(setq projectile-globally-ignored-files '("TAGS" ".git"))
(setq projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" genehack/emacs-tmp-dir))

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
(if (file-exists-p "/opt/ensime")
    (progn
      ;; load the ensime lisp code...
      (add-to-list 'load-path "/opt/ensime/elisp")
      (require 'ensime)

      ;; This step causes the ensime-mode to be started whenever
      ;; scala-mode is started for a buffer. You may have to customize
      ;; this step if you're not using the standard scala mode.
      (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)))


;;; SCRATCH-BUFFER
(defun genehack/create-scratch-buffer nil
  "(re)create a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (insert initial-scratch-message)
  (lisp-interaction-mode))

;;; SMART-TAB
(require 'smart-tab)
(global-smart-tab-mode 1)
(setq smart-tab-using-hippie-expand t)
(setq smart-tab-completion-functions-alist
      '((cperl-mode      . genehack/company-yasnippet-or-completion)
        (emacs-lisp-mode . genehack/company-yasnippet-or-completion)
        (lisp-mode       . genehack/company-yasnippet-or-completion)
        (go-mode         . genehack/company-yasnippet-or-completion)
        (text-mode       . dabbrev-completion)))

;;; SMARTPARENS
(smartparens-global-mode 1)
(show-smartparens-global-mode t)

;;; SMEX
(require 'smex)
(smex-initialize)

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
    lisp-mode
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

;;; TEMPLATE
(require 'template-mode)
(defun genehack/enable-template-minor-mode ()
  "Turn on 'template-minor-mode' in *.tt files."
  (if (string-match "\\.tt2?$" buffer-file-name)
      (template-minor-mode 1)))
(add-hook 'html-mode-hook 'genehack/enable-template-minor-mode)

;;; TEXT-SCALE
(defun genehack/text-scale-default ()
  "Set text scale to default."
  (interactive)
  (text-scale-set 0))

;;; THEME
(require 'solarized)
(defun genehack/solarize-this ()
  "Enable solarized theme."
  (interactive)
  (load-theme 'solarized-dark t))
(defun genehack/solarize-this-light ()
  "Enable solarized theme."
  (interactive)
  (load-theme 'solarized-light t))
(require 'twilight-theme)
(defun genehack/twilight-this ()
  "Enable twilight theme."
  (interactive)
  (load-theme 'twilight t))
(genehack/solarize-this)

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
  (insert (shell-command-to-string (format "u %s" char))))

;;; URL ENCODING
;;;; based on http://twitter.com/#!/OvidPerl/status/28076709865586688
(defun genehack/unescape_uri (begin end)
  "URI unescape region between BEGIN and END."
  (interactive "r")
  (shell-command-on-region
   begin end
   "perl -MURI::Escape -e 'print URI::Escape::uri_unescape(do { local $/; <STDIN> })'"
   'current-buffer t))

;;; YAML-MODE
(autoload 'yaml-mode "yaml-mode" "YAML" t)
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))

;;; YASNIPPET
(eval-when-compile (defvar genehack/emacs-dir))
(require 'yasnippet)
(setq yas-use-menu nil)
(yas--initialize)
(defvar genehack/yas-snippet-dir (concat genehack/emacs-dir "share/snippets")
  "Directory with snippets."
  )
(if (file-exists-p genehack/yas-snippet-dir)
    (unless (file-directory-p genehack/yas-snippet-dir)
      (error "Snippets directory creation blocked by file"))
  (make-directory genehack/yas-snippet-dir))
(yas-load-directory genehack/yas-snippet-dir)
(add-to-list 'yas-snippet-dirs genehack/yas-snippet-dir)
(yas-global-mode)

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
                  (yas--current-key))
              (yas--current-key))))
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

;;; WEB-MODE
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.p?html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tx\\'" . web-mode))

;;; put this at the end so that everything is loaded...
;;; DIMINISH
;;;; from http://whattheemacsd.com/init.el-04.html
(require 'diminish)
(diminish 'company-mode)
(diminish 'projectile-mode)
(diminish 'smart-tab-mode)
(diminish 'yas-minor-mode)

(provide 'misc)
;;; misc.el ends here
