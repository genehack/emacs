;; misc.el -- various customizations and additions
;;; This is for stuff that _isn't_ built-in to Emacs

;;; AUTO COMPLETE
(add-to-list 'load-path (concat genehack/emacs-libs-dir "auto-complete"))
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (concat genehack/emacs-libs-dir "auto-complete/ac-dict"))
(ac-config-default)
(setq ac-comphist-file (concat genehack/emacs-config-dir "tmp/ac-comphist.dat"))

;;; AUTO CREATE DIRECTORIES
;;;; after <http://atomized.org/2008/12/emacs-create-directory-before-saving/>
(add-hook 'before-save-hook
          '(lambda ()
             (or (file-exists-p (file-name-directory buffer-file-name))
                 (make-directory (file-name-directory buffer-file-name) t))))

;;; BROWSE-KILL-RING
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;;; COLOR-THEME
(add-to-list 'load-path (concat genehack/emacs-libs-dir "color-theme"))
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-hober)))

;;; CSS-HEXCOLOR
(require 'css-hexcolor)

;;; DIFF-CURRENT-BUFFER-WITH-FILE
(defun genehack/diff-current-buffer-with-file ()
  "Show diff between current buffer contents and file on disk"
  (interactive)
  (diff-buffer-with-file (current-buffer)))

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

;;; JS2
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq-default js2-basic-offset 2)

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
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.mrkd" . markdown-mode))

;;; MULTI-TERM
(require 'multi-term)
(defalias 'term 'multi-term)
(setq multi-term-dedicated-select-after-open-p t
      multi-term-dedicated-window-height 24)

;;; PAREN-BOUNCE
;;;; ganked from <http://elfs.livejournal.com/1216037.html>
(defun genehack/paren-bounce ()
  (interactive)
  (let ((prev-char (char-to-string (preceding-char)))
	(next-char (char-to-string (following-char))))
    (cond ((string-match "[[{(<]" next-char) (forward-sexp 1))
          ((string-match "[\]})>]" prev-char) (backward-sexp 1))
          (t (error "%s" "Not an expression boundary.")))))
