;; perl.el - perl-specific customizations and code

;;; LIBRARIES
(genehack/add-emacs-lib-subdir-to-load-path "cperl-mode")
(require 'cperl-mode)
(require 'perl-find-library)
(require 'pod-mode)
(require 'prove)
(require 'perlcritic)
(require 'perltidy)

(defalias 'perl-mode 'cperl-mode)

(add-to-list 'auto-mode-alist '("\\.cgi$" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.t$" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.pod$" . pod-mode))

;;; HOOK MODS
(add-hook 'cperl-mode-hook 'genehack/cperl-mode-setup)
(add-hook 'cperl-mode-hook 'autopair-mode)
(add-hook 'cperl-mode-hook 'flyspell-prog-mode)
(add-hook 'cperl-mode-hook 'flymake-mode)
(add-hook 'cperl-mode-hook 'linum-mode)
(add-hook 'cperl-mode-hook 'which-func-mode)

;;; CONFIG
(setq
 cperl-auto-newline t
 cperl-auto-indent-on-semi t
 cperl-close-paren-offset -2
 cperl-continued-statement-offset 2
 ;cperl-hairy t
 cperl-highlight-variables-indiscriminately t
 cperl-indent-level 2
 cperl-indent-parens-as-block t
 cperl-invalid-face nil
 cperl-tab-always-indent t
 cperl-electric-lbrace-space nil ; must needs be after cperl-hairy
 cperl-clobber-lisp-bindings nil ; must needs be after cperl-hairy

 cperl-electric-parens nil
 cperl-font-lock t
 cperl-electric-linefeed t
 cperl-electric-keywords t
 cperl-info-on-command-no-prompt t
 cperl-clobber-lisp-bindings t
 cperl-lazy-help-time 5
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
  ) "list of keybindings to unset in cperl-mode buffers
since cperl-mode steps on a lot of the C-c C-* bindings I use globallly..." )

;;; perl COMPLETION
(setq plcmp-extra-using-modules
      '(("Digest" . "Digest::SHA1")
        ("LWP::UserAgent" . "HTTP::Response")))

;;; UTILITIES
(defun genehack/perl-repl ()
  (interactive)
  "Start or show an re.pl in an ansi-term shell"
  (if (get-buffer "*re.pl*")
      (switch-to-buffer "*re.pl*")
    (ansi-term "re.pl" "re.pl")))

(defun genehack/get-test-or-lib-for-current-file ()
  "Given a lib file, create and return a buffer for the corresponding test lib file,
or vice versa."
  (let ((file-target (shell-command-to-string (format "~/bin/map-test-lib %s" buffer-file-name))))
    (if (file-exists-p file-target)
        (find-file-noselect file-target)
      (make-directory (file-name-directory file-target) t)
      (find-file-noselect file-target t))))

(defun genehack/jump-from-test-to-lib ()
  (interactive)
  "Toggle between a Perl class and the Test class for that class."
  (switch-to-buffer (genehack/get-test-or-lib-for-current-file)))

(defun genehack/split-into-lib-and-test ()
  (interactive)
  "When looking at a lib, convert to horizontal split with lib and test lib."
  (delete-other-windows)
  (split-window-horizontally)
  (switch-to-buffer (genehack/get-test-or-lib-for-current-file)))

(defun genehack/cperl-mode-setup ()
  ;; allows 'M-x compile' for syntax checking of Perl scripts within Emacs
  ;; from e.goerlach@computer.org (Ekkehard Görlach) in comp.emacs
  (set (make-local-variable 'compile-command)
       (concat "perl -cw  " buffer-file-name))
  (font-lock-add-keywords nil '(("^[^\n]\\{90\\}\\(.*\\)$" 1 font-lock-warning-face t)))
  (setq fill-column 78)
  ;;(turn-on-font-lock)
  (dolist (binding genehack/cperl-keybindings-to-remove)
    (local-unset-key (edmacro-parse-keys binding)))
  (define-key cperl-mode-map (kbd "C-c C-i") 'cperl-invert-if-unless)
  (define-key cperl-mode-map (kbd "C-c C-s") 'cperl-pod-spell)
  (define-key cperl-mode-map (kbd "C-c C-\\") 'cperl-lineup)
  (require 'elide-head)
  (add-to-list 'elide-head-headers-to-hide '("######" . "######"))
  (elide-head))

;;; FLYMAKE
;;;; based on a modified version of code found at
;;;; http://blog.urth.org/2011/06/flymake-versus-the-catalyst-restarter.html
(genehack/add-emacs-lib-subdir-to-load-path "flymake-perlcritic")
(setq flymake-perlcritic-severity 5)
(require 'flymake-perlcritic)

;;; PERLY_SENSE
(global-unset-key "\C-z")
(setq ps/key-prefix "\C-z")
(define-key cperl-mode-map (kbd "C-z C-z") 'ps/class-overview-for-class-at-point)

(setq ps/load-flymake nil)
(setq flymake-start-syntax-check-on-find-file t)
(setq flymake-start-syntax-check-on-newline t)

(setq ps/external-dir (shell-command-to-string "/opt/perl/bin/perly_sense external_dir"))

(if (string-match "Devel.PerlySense.external" ps/external-dir)
    (progn
      (message "Loading PerlySense elisp from '%s'..." ps/external-dir)
      (add-to-list 'load-path (format "%s/%s" ps/external-dir "emacs"))
      (require 'perly-sense))
  (message
   "Could not identify PerlySense install dir.
Is Devel::PerlySense installed properly?
Does 'perly_sense external_dir' give you a proper directory?
 (%s)"
   ps/external-dir))
