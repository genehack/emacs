(add-to-list 'load-path (concat genehack/emacs-libs-dir "org/lisp"))
(add-to-list 'load-path (concat genehack/emacs-libs-dir "org/contrib/lisp"))
(require 'org-install)

;; The following lines are always needed.  Choose your own keys.
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-log-done t)

(setq org-startup-indented 1
      org-agenda-files '("~/Dropbox/org/")
      org-directory (expand-file-name "~/Dropbox/org/")
      org-default-notes-file (concat org-directory "/capture.org"))

(define-key global-map "\C-cc" 'org-capture)
