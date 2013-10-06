;; builtins.el -- Customizations of built-in (non-third party) options.
;;; large packages (eg Gnus, ERC, etc.) should get dedicated files but
;;; for standard stuff where one or two options get frobbed, this is
;;; the place.

;; ANSI-MODE FOR SHELLS
(ansi-color-for-comint-mode-on)

;; AUTO-SAVE BACKUPS
(setq auto-save-list-file-prefix (concat genehack/emacs-tmp-dir "auto-save-list/.saves-"))
(setq make-backup-files nil)

;;; CALENDAR
(setq mark-holidays-in-calendar t)

;;; CURSOR
(setq-default cursor-type 'box)

;;; DEBUGGING
(setq debug-on-error t)

;;; DESKTOP
(desktop-save-mode 1)

;;; DIRED
(require 'dired)
(defun jsja-dired-right-here ()
  "Run dired on current active directory."
  (interactive)
  (dired default-directory))

(autoload 'dired-jump "dired-x"
  "Jump to Dired buffer corresponding to current buffer." t)
(define-key global-map "\C-x\C-j" 'dired-jump)

;;;; http://whattheemacsd.com//setup-dired.el-02.html
(defun dired-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 4))

(define-key dired-mode-map
  (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)

(defun dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

(define-key dired-mode-map
  (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)

;;;; http://www.reddit.com/r/emacs/comments/18qa15/dired_discussion/
(require 'dired-details)
(require 'dired-details+)
(dired-details-install)
(defun genehack/bind-key-for-wdired ()
  "Add a keybinding for wdired in dired-mode"
  (local-set-key (kbd "E") 'wdired-change-to-wdired-mode))
(add-hook 'dired-mode-hook 'genehack/bind-key-for-wdired)
(setq-default dired-listing-switches "-alhv --time-style=long-iso")
(setq dired-recursive-copies 'always)

;;; DISABLE
(put 'overwrite-mode 'disabled t)

;;; EXECUTABLE-UPON-SAVE MAGIC
;;  from <http://www.emacswiki.org/cgi-bin/wiki/MakingScriptsExecutableOnSave>
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;;; FFAP
(require 'ffap)
(ffap-bindings)

;;; FONT-LOCK
(require 'font-lock)
(global-font-lock-mode 1)
(setq-default font-lock-maximum-decoration t
              font-lock-maximum-size nil)
(setq jit-lock-stealth-time 5
      jit-lock-defer-contextually t
      jit-lock-stealth-nice 0.5)

;;; GENERAL INDENTATION RELATED OPTIONS
(setq-default indent-tabs-mode nil)

(defvar modes-for-indentation-munging
  '(c++-mode
    c-mode
    cperl-mode
    emacs-lisp-mode
    objc-mode
    python-mode
    rspec-mode
    ruby-mode
    scala-mode)
  "List of modes to set up to do indent-on-paste and
remove-leading-whitespace-on-kil-line tricks")

;; re-indent when pasting back into programming-related major modes
;; from <http://www.emacswiki.org/emacs-en/AutoIndentation>
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command
           (after indent-region activate)
           (and (not current-prefix-arg)
                (member major-mode modes-for-indentation-munging)
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))

;; remove excess white space when killing newlines in
;; programming-related major modes
;; from <http://www.emacswiki.org/emacs-en/AutoIndentation>
(defadvice kill-line (before check-position activate)
  (if (member major-mode modes-for-indentation-munging)
      (if (and (eolp) (not (bolp)))
          (progn (forward-char 1)
                 (just-one-space 0)
                 (backward-char 1)))))

;;; GLOBAL AUTO-REVERT
(global-auto-revert-mode t)
;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;;; HIGHLIGHTING
;;(set-face-foreground 'highlight "gray10")
;;(set-face-background 'highlight "yellow")

;;; HIPPY-EXPAND
(setq hippie-expand-try-functions-list '(
                                         try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol
                                         ))
(setq hippie-expand-dabbrev-skip-space t)

;;; HTML
(add-hook 'html-mode-hook 'turn-off-auto-fill)

;;; IBUFFER
(require 'ibuffer)
(setq ibuffer-default-sorting-mode 'major-mode)

;;; IDO
(require 'ido)
(setq ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-file-extensions-order '(".pm" ".pl" ".tt" ".tt2" ".html" ".css" t))
(ido-everywhere)
(ido-mode 1)

;;; IMENU
(require 'imenu)
(setq imenu-auto-rescan t)
(defun imenu-goto-symbol ()
  "Will update the imenu index and then use ido to select a symbol to navigate to"
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols (symbol-list)
                       (when (listp symbol-list)
                         (dolist (symbol symbol-list)
                           (let ((name nil) (position nil))
                             (cond
                              ((and (listp symbol) (imenu--subalist-p symbol))
                               (addsymbols symbol))
                              ((listp symbol)
                               (setq name (car symbol))
                               (setq position (cdr symbol)))
                              ((stringp symbol)
                               (setq name symbol)
                               (setq position (get-text-property 1 'org-imenu-marker symbol))))
                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (if (markerp position)
          (goto-char position) (goto-char (overlay-start position))))))

;;; KEYSTROKE ECHO
(setq echo-keystrokes 0.1)

;;; LINE NUMBERS
(column-number-mode 1)
(defvar genehack/linum-max-line-width "0"
  "number of digits in last line in current buffer.
This is a buffer-local variable.")
(defun genehack/linum-before-numbering ()
  "Small kludge to figure out the appropriate width for linum to use."
  (make-local-variable 'genehack/linum-max-line-width)
  (save-excursion
    (goto-char (point-max))
    (setq genehack/linum-max-line-width (length (format "%s" (line-number-at-pos))))))
(add-hook 'linum-before-numbering-hook 'genehack/linum-before-numbering)
(defun genehack/linum-format (number)
  "My linum format"
  (format (concat " %" (number-to-string genehack/linum-max-line-width) "d ") number))
(setq linum-format 'genehack/linum-format)

;;; MAC STUFF
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-function-modifier 'super)
  (setq mac-option-modifier nil)
  (add-to-list 'ido-ignore-files "\\.DS_Store")
  ;;  (ns-toggle-fullscreen)
  (setq browse-url-browser-function 'browse-url-default-macosx-browser)
  (setq delete-by-moving-to-trash t))

;;; MESSAGE LOG
(setq message-log-max 5000)

;;; (MENU/SCROLLBAR/TOOLBAR)-MODE
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;;; NXML-MODE
(fset 'xml-mode 'nxml-mode)
(add-to-list 'auto-mode-alist '("\\.rng'"  . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.rss'"  . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xml'"  . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xsd'"  . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xslt'" . nxml-mode))

(if (boundp 'magic-mode-alist)
    (setq magic-mode-alist (cons '("<\\?xml " . nxml-mode) magic-mode-alist))
  (defvar magic-mode-alist)
  (setq magic-mode-alist '("<\\?xml " . nxml-mode)))

(setq nxml-bind-meta-tab-to-complete-flag nil
      nxml-syntax-highlight-flag t)

;;; PAREN MATCH
(require 'paren)
(show-paren-mode t)
(setq show-paren-style 'expression)

;;; PS PRINT
(setq ps-print-color-p nil)

;;; SAVE-HIST
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(setq savehist-file (expand-file-name "savehist" genehack/emacs-tmp-dir))
(savehist-mode t)

;;; SAVEPLACE
(setq save-place-file (expand-file-name "saveplace" genehack/emacs-tmp-dir))
(setq-default save-place t)
(require 'saveplace)

;;; SCRATCH BUFFER
(setq initial-scratch-message
      "welcome to john's emacs.\n\nput additional stuff in initial-scratch-message to have it show up here.\n\n")

;;; SERVER
(server-start)

;;; SGML-MODE
(require 'sgml-mode)
(define-key sgml-mode-map "\C-c\C-b" nil)

;;; SIZE INDICATION MODE
(size-indication-mode t)

;;; SPELL CHECKING
;;  (note that exec-path probably needs to be munged before this is run)
(defun genehack/find-in-exec-path (program)
  (let ((found nil))
    (dolist (path exec-path)
      (if (file-exists-p (concat path "/" program))
          (setq found t)))
    found))

(defun genehack/spelling-not-found ()
  (interactive)
  (message "Spell check not enabled; neither aspell nor ispell found in path."))

(defvar genehack/found-spelling-program nil
  "Boolean indicating whether or not a spelling program was found in exec-path")

(let ()
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
(defun genehack/turn-off-yas-mode ()
  "Turn off yas-mode"
  (yas-minor-mode -1))
(add-hook 'term-mode-hook 'genehack/turn-off-yas-mode)

;;; TEXT-MODE
(defun genehack/set-up-text-mode ()
  "My customizations for text-mode"
  (require 'filladapt)
  (auto-fill-mode 1)
  (filladapt-mode 1)
  (if (eq genehack/found-spelling-program t)
      (flyspell-mode 1)))
(add-hook 'text-mode-hook 'genehack/set-up-text-mode)

;;; TIME DISPLAY
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time)

;;; TITLE BARS
(setq frame-title-format "<%b> == (%f) [mode: %m]")

;;; TRANSIENT MARK MODE
(transient-mark-mode 1)

;;; TRASH
(setq delete-by-moving-to-trash t)

;;; UNIQUIFY
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse
      uniquify-separator "/"
      uniquify-after-kill-buffer-p t)

;;; UTF8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;; VC
(setq vc-follow-symlinks t)

;;; WGET
(setq wget-download-directory "~/tmp")

;;; YANK
(setq-default mouse-yank-at-point t)

;;; YES-OR-NO
(defalias 'yes-or-no-p 'y-or-n-p)
