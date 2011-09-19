;; perl.el - perl-specific customizations and code

;;; LIBRARIES
(genehack/add-emacs-lib-subdir-to-load-path "cperl-mode")
(require 'cperl-mode)
(require 'perl-completion)
(require 'perl-find-library)
(require 'pod-mode)
(require 'prove)
(require 'tt-mode)
(require 'perlcritic)
(require 'perltidy)

(defalias 'perl-mode 'cperl-mode)

(add-to-list 'auto-mode-alist '("\\.cgi$" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.t$" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.pod$" . pod-mode))

;;; HOOK MODS
(add-hook 'cperl-mode-hook 'genehack/cperl-mode-setup)
(add-hook 'cperl-mode-hook 'genehack/perl-auto-complete-setup)
(add-hook 'cperl-mode-hook 'flyspell-prog-mode)
(add-hook 'cperl-mode-hook 'flymake-mode)
(add-hook 'cperl-mode-hook 'linum-mode)

;;; CONFIG
(setq
 cperl-auto-newline t
 cperl-auto-indent-on-semi t
 cperl-close-paren-offset -2
 cperl-continued-statement-offset 2
 cperl-hairy t
 cperl-highlight-variables-indiscriminately t
 cperl-indent-level 2
 cperl-indent-parens-as-block t
 cperl-invalid-face nil
 cperl-tab-always-indent t
 cperl-electric-lbrace-space nil ; must needs be after cperl-hairy
 cperl-clobber-lisp-bindings nil ; must needs be after cperl-hairy
 )

;;; PERL COMPLETION
(setq plcmp-extra-using-modules
      '(("Digest" . "Digest::SHA1")
        ("LWP::UserAgent" . "HTTP::Response")))

;;; UTILITIES
(defun genehack/jump-from-test-to-lib ()
  (interactive)
  (let ((test-file buffer-file-name))
    (find-file (shell-command-to-string (format "map-test-lib %s" test-file)))))

(defun genehack/cperl-mode-setup ()
  ;; allows 'M-x compile' for syntax checking of Perl scripts within Emacs
  ;; from e.goerlach@computer.org (Ekkehard Görlach) in comp.emacs
  (set (make-local-variable 'compile-command)
       (concat "perl -cw  " buffer-file-name))
  (font-lock-add-keywords nil '(("^[^\n]\\{90\\}\\(.*\\)$" 1 font-lock-warning-face t)))
  (setq fill-column 78)
  ;;(turn-on-font-lock)
  )

(defun genehack/perl-auto-complete-setup ()
  (perl-completion-mode t)
  (define-key cperl-mode-map (kbd "C-c C-3") 'comment-region)
  (when (require 'auto-complete nil t) ; no error whatever auto-complete.el is not installed.
    (auto-complete-mode t)
    (setq ac-sources
          '(ac-source-perl-completion
            ac-source-yasnippet
            ac-source-abbrev
            ac-source-words-in-buffer))))

;;; FIX INDENTATION
;;;; from http://www.emacswiki.org/emacs-en/IndentingPerl
;; (load-library "cperl-mode")
;; (defun cperl-backward-to-start-of-continued-exp (lim)
;;   (if (memq (preceding-char) (append ")]}\"'`" nil))
;;       (forward-sexp -1))
;;   (beginning-of-line)
;;   (if (or (<= (point) lim) (< 0 cperl-continued-statement-offset))
;;       (goto-char (1+ lim)))
;;   (skip-chars-forward " \t"))

;;; FLYMAKE
;;;; from http://blog.urth.org/2011/06/flymake-versus-the-catalyst-restarter.html
(defun flymake-perl-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-intemp))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "/opt/perl/bin/perl" (list "-MProject::Libs" "-wc" local-file))))

(defun flymake-create-temp-intemp (file-name prefix)
  "Return file name in temporary directory for checking
   FILE-NAME. This is a replacement for
   `flymake-create-temp-inplace'. The difference is that it gives
   a file name in `temporary-file-directory' instead of the same
   directory as FILE-NAME.

   For the use of PREFIX see that function.

   Note that not making the temporary file in another directory
   \(like here) will not if the file you are checking depends on
   relative paths to other files \(for the type of checks flymake
   makes)."
  (unless (stringp file-name)
    (error "Invalid file-name"))
  (or prefix
      (setq prefix "flymake"))
  (let* ((name (concat
                (file-name-nondirectory
                 (file-name-sans-extension file-name))
                "_" prefix))
         (ext  (concat "." (file-name-extension file-name)))
         (temp-name (make-temp-file name nil ext))
         )
    (flymake-log 3 "create-temp-intemp: file=%s temp=%s" file-name temp-name)
    temp-name))

(setq temporary-file-directory "~/.emacs.d/tmp/")

;;; PERLY_SENSE
(global-unset-key "\C-z")
(setq ps/key-prefix "\C-z")
(define-key cperl-mode-map (kbd "C-z C-z") 'ps/class-overview-for-class-at-point)

;(setq perly-sense-load-flymake t)
;(setq ps/load-flymake t)
(setq flymake-start-syntax-check-on-find-file t)
(setq flymake-start-syntax-check-on-newline t)

(setq ps/external-dir (shell-command-to-string "perly_sense external_dir"))

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

;;(setq ps/use-prepare-shell-command nil)

