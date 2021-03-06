;;; builtins.el -- Customizations of built-in (non-third party) options.

;;; Commentary:
;;;; Large packages (eg Gnus, ERC, etc.) should get dedicated files but
;;;; for standard stuff where one or two options get frobbed, this is
;;;; the place.

;;; Code:

(require 'use-package)

;;; ANSI-MODE FOR SHELLS
(ansi-color-for-comint-mode-on)

;;; AUTO-SAVES AND BACKUPS
(eval-when-compile (defvar genehack/emacs-tmp-dir))
(setq auto-save-list-file-prefix (concat genehack/emacs-tmp-dir "auto-save-list/.saves-"))
(setq auto-save-file-name-transforms `((".*" ,genehack/emacs-tmp-dir t)))
(setq backup-by-copying t)
(defvar genehack/backup-dir (concat genehack/emacs-tmp-dir "saves/" )
  "Place to put file backups.")
(setq backup-directory-alist `((".*" . ,genehack/backup-dir)))
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq version-control t)

;;; BUFFER-MODE
(defun genehack/set-up-buffer-mode ()
  "My customizations for 'Buffer-menu-mode'."
  (setq show-trailing-whitespace nil))
(add-hook 'Buffer-menu-mode-hook 'genehack/set-up-buffer-mode)

;;; CALENDAR
(use-package calendar
  :defer t
  :config
  (setq calendar-mark-holidays-flag t))

;;; CURSOR
(setq-default cursor-type 'box)

;;; DEBUGGING
(setq debug-on-error t)

;;; DESKTOP
(desktop-save-mode 1)

;;; DIRED
(defun genehack/bind-key-for-wdired ()
  "Add a keybinding for wdired in 'dired-mode'."
  (local-set-key (kbd "E") 'wdired-change-to-wdired-mode))

;;;; having all-the-icons mode on is pretty, but having the icons in
;;;; the buffer fucks up wdired something fierce. let's defadvice the
;;;; functions that take us into and out of wdired mode so they remove
;;;; the icons and then put them back.
(defadvice wdired-change-to-wdired-mode (before genehack/disable-icons activate)
  "Disable all-the-icons when entering wdired."
  (all-the-icons-dired-mode 0))
(defadvice wdired-finish-edit (after genehack/restore-icons activate)
  "Restore all-the-icons after an edit."
  (interactive)
  (all-the-icons-dired-mode 1))


;;;; these are in 'builtins' because they modify dired, which is a
;;;; built-in mode
(use-package all-the-icons :ensure t)
(use-package all-the-icons-dired :ensure t)

(use-package dired
  :after all-the-icons-dired
  :defer t
  :commands dired
  :config
  (define-key dired-mode-map (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)
  (define-key dired-mode-map (vector 'remap 'end-of-buffer)       'dired-jump-to-bottom)
  (add-hook 'dired-mode-hook 'genehack/bind-key-for-wdired)
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  (setq-default dired-listing-switches "-alhv --time-style=long-iso")
  (setq dired-recursive-copies 'always))

(use-package dired-x
  :after dired
  :defer t
  :commands dired-jump)

;;;; http://whattheemacsd.com//setup-dired.el-02.html
(require 'dired)
(defun dired-back-to-top ()
  "Jump to the top file in a dired buffer."
  (interactive)
  (goto-char (point-min))
  ;; because the number of header lines varies depending on whether
  ;; mode info is shown or hidden, find the double-dot directory entry
  ;; and go forward one line -- heuristic, but will always work.
  (search-forward "..")
  (dired-next-line 1))

(defun dired-jump-to-bottom ()
  "Jump to the last file in a dired buffer."
  (interactive)
  (goto-char (point-max))
  (dired-next-line -1))

;;; DISABLE / ENABLE
(put 'downcase-region 'disabled nil)
(put 'overwrite-mode 'disabled t)
(put 'upcase-region 'disabled nil)

;;; ELDOC
(use-package eldoc
  :defer t
  :diminish (eldoc-mode . " el"))

;;; EXECUTABLE-UPON-SAVE MAGIC
;;;; from <http://www.emacswiki.org/cgi-bin/wiki/MakingScriptsExecutableOnSave>
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;;; FFAP
(use-package ffap
  :disabled t
  :defer t
  :config
  (ffap-bindings))

;;; FONT-LOCK
(use-package font-lock
  :defer t
  :config
  (setq-default font-lock-maximum-decoration t
                font-lock-maximum-size nil)
  (setq jit-lock-stealth-time 5
        jit-lock-defer-contextually t
        jit-lock-stealth-nice 0.5)
  :init
  (global-font-lock-mode 1))

;;; FRAME RESIZE
;;;; see https://www.reddit.com/r/emacs/comments/f3ed3r/how_is_doom_emacs_so_damn_fast/fhicvbj/
(setq frame-inhibit-implied-resize t)

;;; GENERAL INDENTATION RELATED OPTIONS
(setq-default indent-tabs-mode nil)
(setq tab-always-indent 'complete)
(setq sentence-end-double-space nil)

(defvar modes-for-indentation-munging
  '(c++-mode
    c-mode
    cperl-mode
    emacs-lisp-mode
    go-mode
    js-mode
    js2-mode
    objc-mode
    python-mode
    rspec-mode
    ruby-mode
    scala-mode
    web-mode)
  "List of modes to set up to do indent-on-paste.
Also remove-leading-whitespace-on-kill-line tricks")

;;;; re-indent when pasting back into programming-related major modes
;;;; from <http://www.emacswiki.org/emacs-en/AutoIndentation>
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command
           (after indent-region activate)
           (and (not current-prefix-arg)
                (member major-mode modes-for-indentation-munging)
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))

;;;; from <http://www.emacswiki.org/emacs-en/AutoIndentation>
(defadvice kill-line (before check-position activate)
  "Remove excess white space when killing newlines in programming-related major modes."
  (if (member major-mode modes-for-indentation-munging)
      (if (and (eolp) (not (bolp)))
          (progn (forward-char 1)
                 (just-one-space 0)
                 (backward-char 1)))))

;;; GLOBAL AUTO-REVERT
(use-package autorevert
  :defer t
  :diminish (autorevert-mode . " a")
  :config
  ;; Also auto refresh dired, but be quiet about it
  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-verbose nil)
  :init
  (global-auto-revert-mode t))

;;; HTML
(add-hook 'html-mode-hook 'turn-off-auto-fill)

;;; IBUFFER
;;;; this is in 'builtins' because it modifies ibuffer, which is a
;;;; built-in mode
(use-package all-the-icons-ibuffer
  :ensure t
  :defer t
  :init (all-the-icons-ibuffer-mode 1))
(use-package ibuffer
  :defer t
  :config
  (setq ibuffer-default-sorting-mode 'major-mode))

;;; KEYSTROKE ECHO
(setq echo-keystrokes 0.1)

;;; LINE NUMBERS
(column-number-mode 1)

;;; MAC STUFF
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil)
  (setq browse-url-browser-function 'browse-url-default-macosx-browser)
  (setq delete-by-moving-to-trash t))

;;; MESSAGE LOG
(setq message-log-max 5000)

;;; (MENU/SCROLLBAR/TOOLBAR)-MODE
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;;; MOUSE WHEEL
(if (fboundp 'mouse-wheel-mode) (mouse-wheel-mode -1))

;;; NXML-MODE
(use-package nxml-mode
  :defer t
  :config
  (fset 'xml-mode 'nxml-mode)
  (setq nxml-bind-meta-tab-to-complete-flag nil)
  :mode "\\.\\(rng\\|rss\\|xml\\|xsd\\|xslt\\)\\'"
  :magic "<\\?xml ")

;;; ORG MODE
(use-package org
  :defer t
  :disabled t
  :defines
  org-agenda-custom-commands
  org-agenda-files
  org-capture-templates
  org-default-notes-file
  org-directory
  org-log-done
  org-refile-targets
  org-return-follows-link
  org-tag-alist
  org-todo-keywords
  :config
  (setq org-agenda-custom-commands
        '(("a" "active" todo "ACTIVE")
          ("n" "next actions" tags-todo "NEXT")
          ("w" "waiting for" todo "WAITING")))
  (setq org-agenda-files '("~/org" "~/org/home" "~/org/oss" "~/org/work"))
  (setq org-capture-templates
        '(("t" "todo" entry (file+headline "" "* INBOX") "** TODO %?\n %i\n %a")))
  (setq org-default-notes-file (concat org-directory "/jfdi.org"))
  (setq org-log-done 'time)
  (setq org-refile-targets '((org-agenda-files . (:maxlevel . 2))))
  (setq org-return-follows-link t)
  (setq org-tag-alist '(("@work" . ?w) ("@home" . ?h) ("@oss" . ?o) ("NEXT" . ?n)))
  (setq org-todo-keywords '((sequence "TODO(t)" "ACTIVE(a)" "|" "DONE(d!)")
                            (sequence "WAITING(w!/@)" "|")))
  (defun jfdi ()
    "JFDI!"
    (interactive)
    (find-file "~/org/jfdi.org")))

;;; PAREN MATCH
(use-package paren
  :defer t
  :config
  (show-paren-mode t)
  (setq show-paren-style 'expression))

;;; PS PRINT
(setq ps-print-color-p nil)

;;; SAVE-HIST
(use-package savehist
  :defer t
  :config
  (setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring)
        savehist-file (expand-file-name "savehist" genehack/emacs-tmp-dir))
  :init
  (savehist-mode t))

;;; SAVEPLACE
(use-package saveplace
  :defer t
  :config
  (setq save-place-file (expand-file-name "saveplace" genehack/emacs-tmp-dir))
  :init
  (setq-default save-place t))

;;; SERVER
(server-start)

;;; SGML-MODE
(use-package sgml-mode
  :defer t
  :config
  (define-key sgml-mode-map "\C-c\C-b" nil))

;;; SIZE INDICATION MODE
(size-indication-mode t)

;;; SPELL CHECKING
;;;; (note that exec-path probably needs to be munged before this is run)
(defun genehack/find-in-exec-path (program)
  "Find PROGRAM in 'exec-path'."
  (let ((found nil))
    (dolist (path exec-path)
      (if (file-exists-p (concat path "/" program))
          (setq found t)))
    found))

(defun genehack/spelling-not-found ()
  "Display message when *spell program can't be found."
  (interactive)
  (message "Spell check not enabled; neither aspell nor ispell found in path."))

(defvar genehack/found-spelling-program nil
  "Boolean indicating whether or not a spelling program was found in 'exec-path'.")

(use-package ispell
  :defer t
  :config
  (if (genehack/find-in-exec-path "aspell")
      (progn
        (setq-default ispell-program-name "aspell")
        (setq ispell-extra-args '("--sug-mode=ultra"))
        (setq genehack/found-spelling-program t))
    (if (genehack/find-in-exec-path "ispell")
        (progn
          (setq-default ispell-program-name "ispell")
          (setq ispell-extra-args '("-W 3"))
          (setq genehack/found-spelling-program t))))
  (if (eq genehack/found-spelling-program t)
      (progn
        (autoload 'ispell-word   "ispell" "check word spelling."   t)
        (autoload 'ispell-region "ispell" "check region spelling." t)
        (autoload 'ispell-buffer "ispell" "check buffer spelling." t)
        (require 'flyspell))
    (defalias 'ispell-word   'genehack/spelling-not-found)
    (defalias 'ispell-region 'genehack/spelling-not-found)
    (defalias 'ispell-buffer 'genehack/spelling-not-found)))

;;; TERM-MODE
(use-package term
  :defer t
  :config
  (add-hook 'term-mode-hook 'genehack/set-up-term-mode))

(defun genehack/set-up-term-mode ()
  "My customizations for 'term-mode'."
  (yas-minor-mode -1)
  (setq show-trailing-whitespace nil)
  (setq term-buffer-maximum-size 10000))

;;; TEXT-MODE
(defun genehack/set-up-text-mode ()
  "My customizations for 'text-mode'."
  (require 'filladapt)
  (auto-fill-mode 1)
  (filladapt-mode 1)
  (if (eq genehack/found-spelling-program t)
      (flyspell-mode 1)))
(add-hook 'text-mode-hook 'genehack/set-up-text-mode)

;;; TIME DISPLAY
(use-package time
  :defer t
  :defines
  display-time-24hr-format
  :config
  (setq display-time-24hr-format t)
  (setq display-time-day-and-date t)
  :init
  (display-time))

;;; TITLE BARS
(setq frame-title-format "<%b> == (%f) [mode: %m]")

;;; TRANSIENT MARK MODE
(transient-mark-mode 1)

;;; TRASH
(setq delete-by-moving-to-trash t)

;;; UNIQUIFY
(use-package uniquify
  :defer t
  :config
  (setq uniquify-buffer-name-style 'reverse
        uniquify-separator "/"
        uniquify-after-kill-buffer-p t))

;;; UTF8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;; VC
(setq vc-follow-symlinks t)

;;; WHITESPACE
(use-package whitespace
  :defer t
  :config
  (setq whitespace-style '(face tabs spaces trailing lines-tail
                                space-before-tab newline indentation empty
                                space-after-tab space-mark tab-mark newline-mark)))

;;; YANK
(setq-default mouse-yank-at-point t)

;;; YES-OR-NO
(defalias 'yes-or-no-p 'y-or-n-p)

(provide 'builtins)
;;; builtins.el ends here
