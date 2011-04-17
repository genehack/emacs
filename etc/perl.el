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
(add-to-list 'auto-mode-alist '("\\.pod$" . pod-mode))

;;; HOOK MODS
(add-hook 'cperl-mode-hook
  '(lambda ()
     ;; allows 'M-x compile' for syntax checking of Perl scripts within Emacs
     ;; from e.goerlach@computer.org (Ekkehard Görlach) in comp.emacs
     (set (make-local-variable 'compile-command)
          (concat "perl -cw  " buffer-file-name))
     (font-lock-add-keywords nil '(("^[^\n]\\{90\\}\\(.*\\)$" 1 font-lock-warning-face t)))
     (setq fill-column 78)
     (turn-on-font-lock)
     (perl-completion-mode t)
     (when (require 'auto-complete nil t) ; no error whatever auto-complete.el is not installed.
       (auto-complete-mode t)
       (make-variable-buffer-local 'ac-sources)
       (setq ac-sources
             '(ac-source-yasnippet
               ac-source-perl-completion
               ac-source-abbrev
               ac-source-words-in-buffer)))))

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


;;; PERLY_SENSE
;;;; commented out for now because it throws various errors with
;;;; perl-5.12, and the stuff going to STDERR really bollixes up
;;;; Emacs...
;; (global-unset-key "\C-o")
;; (setq ps/key-prefix "\C-o")

;; (setq ps/load-flymake nil)
;; (setq flymake-start-syntax-check-on-find-file nil)
;; (setq flymake-start-syntax-check-on-newline t)

;; (setq ps/external-dir (shell-command-to-string "perly_sense external_dir"))
;; (if (string-match "Devel.PerlySense.external" ps/external-dir)
;;     (progn
;;       (message "Loading PerlySense elisp from '%s'..." ps/external-dir)
;;       (add-to-list 'load-path (format "%s/%s" ps/external-dir "emacs"))
;;       (require 'perly-sense))
;;   (message
;;    "Could not identify PerlySense install dir.
;; Is Devel::PerlySense installed properly?
;; Does 'perly_sense external_dir' give you a proper directory?
;;  (%s)"
;;    ps/external-dir))

;; (setq ps/use-prepare-shell-command t)

