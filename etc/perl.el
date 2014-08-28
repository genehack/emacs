;; perl.el --- perl-specific customizations and code

;;; Commentary:

;;; Code:

;;; LIBRARIES
(require 'cperl-mode)
(require 'perl-find-library)
(require 'pod-mode)
(require 'prove)
(require 'perlcritic)
(require 'perltidy)

(defalias 'perl-mode 'cperl-mode)

(add-to-list 'auto-mode-alist '("\\.cgi$" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.psgi$" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.t$" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.pod$" . pod-mode))

;;; HOOK MODS
(add-hook 'cperl-mode-hook 'genehack/cperl-mode-setup)
(add-hook 'cperl-mode-hook 'flyspell-prog-mode)
(add-hook 'cperl-mode-hook 'which-func-mode)

;;; CONFIG
(setq
 cperl-autoindent-on-semi t
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
 cperl-tab-always-indent t
 )

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
  "Start or show an re.pl in an 'ansi-term' shell."
  (interactive)
  (if (get-buffer "*re.pl*")
      (switch-to-buffer "*re.pl*")
    (ansi-term "re.pl" "re.pl")))

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
  "Munges 'compile' for syntax checking of Perl scripts within Emacs.
From e.goerlach@computer.org (Ekkehard Görlach) in comp.emacs."
  (set (make-local-variable 'compile-command)
       (concat "perl -cw  " buffer-file-name))
  (set (make-local-variable 'flycheck-checker) 'perl-with-lib-from-project-root)
  (font-lock-add-keywords nil '(("^[^\n]\\{90\\}\\(.*\\)$" 1 font-lock-warning-face t)))
  (setq fill-column 78)
  (dolist (binding genehack/cperl-keybindings-to-remove)
    (local-unset-key (edmacro-parse-keys binding)))
  (define-key cperl-mode-map (kbd "C-c C-i") 'cperl-invert-if-unless)
  (define-key cperl-mode-map (kbd "C-c C-s") 'cperl-pod-spell)
  (define-key cperl-mode-map (kbd "C-c C-\\") 'cperl-lineup))

(defun genehack/perl-find-file-at-point ()
  "Find a perl library by module name"
  (interactive)
  (find-file (perl-library-path (cperl-word-at-point))))


(provide 'perl)
;;; perl.el ends here
