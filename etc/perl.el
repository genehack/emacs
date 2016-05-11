;; perl.el --- perl-specific customizations and code

;;; Commentary:

;;; Code:

;;; LIBRARIES
(require 'use-package)
(use-package cperl-mode
  :config (progn
            (defalias 'perl-mode 'cperl-mode)
            (add-hook 'cperl-mode-hook 'genehack/cperl-mode-setup)
            (add-hook 'cperl-mode-hook 'flyspell-prog-mode)
            (add-hook 'cperl-mode-hook 'which-func-mode)
            (setq cperl-autoindent-on-semi t
                  cperl-auto-newline t
                  cperl-clobber-lisp-bindings t
                  cperl-close-paren-offset -2
                  cperl-continued-statement-offset 2
                  cperl-electric-keywords t
                  cperl-electric-lbrace-space nil
                  cperl-electric-linefeed t
                  cperl-electric-parens nil
                  cperl-font-lock t
                  cperl-highlight-variables-indiscriminately t
                  cperl-indent-level 2
                  cperl-indent-parens-as-block t
                  cperl-indent-region-fix-constructs nil
                  cperl-info-on-command-no-prompt t
                  cperl-invalid-face nil
                  cperl-lazy-help-time 5
                  cperl-tab-always-indent t))
  :init
  :mode "\\.\\(cgi\\|psgi\\|t\\)$")

(use-package perl-find-library
  :ensure genehack-perl-elisp)

(use-package pod-mode
  :ensure genehack-perl-elisp
  :mode "\\.pod$")

(use-package prove
  :ensure genehack-perl-elisp)

(use-package perlcritic
  :ensure perlcritic)

(use-package perltidy
  :ensure genehack-perl-elisp)

(defvar genehack/cperl-keybindings-to-remove
  '(
    "\C-c\C-a"
    "\C-c\C-b"
    "\C-c\C-d"
    "\C-c\C-hP"
    "\C-c\C-ha"
    "\C-c\C-hp"
    "\C-c\C-j"
    "\C-c\C-t"
    "\C-c\C-v"
    "\C-c\C-x"
    ) "List of keybindings to unset in 'cperl-mode' buffers.
since 'cperl-mode' steps on a lot of the bindings I use globally..." )

;;; UTILITIES
(defun genehack/perl-repl ()
  "Start or show a reply REPL in an 'ansi-term' shell."
  (interactive)
  (if (get-buffer "*reply*")
      (switch-to-buffer "*reply*")
    (ansi-term "reply" "reply")))

(defun genehack/get-test-or-lib-for-current-file ()
  "Given a lib file, create and return a buffer for the corresponding test lib.
Or vice versa."
  (let ((file-target (shell-command-to-string (format "~/bin/map-test-lib %s" buffer-file-name))))
    (if (file-exists-p file-target)
        (find-file-noselect file-target)
      (make-directory (file-name-directory file-target) t)
      (find-file-noselect file-target t))))

(defun genehack/jump-from-test-to-lib ()
  "Toggle between a Perl class and the Test class for that class."
  (interactive)
  (switch-to-buffer (genehack/get-test-or-lib-for-current-file)))

(defun genehack/split-into-lib-and-test ()
  "When looking at a lib, convert to horizontal split with lib and test lib."
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (switch-to-buffer (genehack/get-test-or-lib-for-current-file)))

(require 'flycheck)
(defun genehack/cperl-mode-setup ()
  "Set up `cperl-mode` for genehack."
  (set (make-local-variable 'compile-command)
       (concat "perl -cw  " buffer-file-name))
  (set (make-local-variable 'flycheck-checker) 'perl-with-lib-from-project-root)
  (font-lock-add-keywords nil '(("^[^\n]\\{90\\}\\(.*\\)$" 1 font-lock-warning-face t)))
  (setq fill-column 78)
  (add-to-list 'safe-local-variable-values
               '(cperl-close-paren-offset . -4)
               '(whitespace-line-column . 132))

  (dolist (binding genehack/cperl-keybindings-to-remove)
    (local-unset-key (edmacro-parse-keys binding)))
  (define-key cperl-mode-map (kbd "C-c C-i") 'cperl-invert-if-unless)
  (define-key cperl-mode-map (kbd "C-c C-s") 'cperl-pod-spell)
  (define-key cperl-mode-map (kbd "C-c C-\\") 'cperl-lineup))

(defun genehack/perl-find-file-at-point ()
  "Find a perl library by module name."
  (interactive)
  (find-file (perl-library-path (cperl-word-at-point))))

(defun pod (word)
  "Run `cpandoc' on WORD.
Largely ganked from `cperl-perldoc' in cperl.el."
  (interactive
   (list (let* ((default-entry (cperl-word-at-point))
                (input (read-string
                        (format "perldoc entry%s: "
                                (if (string= default-entry "")
                                    ""
                                  (format " (default %s)" default-entry))))))
           (if (string= input "")
               (if (string= default-entry "")
                   (error "No perldoc args given")
                 default-entry)
             input))))
  (require 'man)
  (let* ((case-fold-search nil)
         (is-func (and
                   (string-match "^[a-z]+$" word)
                   (string-match (concat "^" word "\\>")
                                 (documentation-property
                                  'cperl-short-docs
                                  'variable-documentation))))
         (Man-switches "")
         (manual-program (if is-func "cpandoc -f" "cpandoc")))
    (Man-getpage-in-background word)))

(provide 'perl)
;;; perl.el ends here
