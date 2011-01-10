(add-to-list 'load-path (concat genehack/emacs-libs-dir "org/lisp"))
(add-to-list 'load-path (concat genehack/emacs-libs-dir "org/contrib/lisp"))
(require 'org-install)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(setq org-log-done t
      org-startup-indented 1
      org-agenda-files '("~/Dropbox/org/")
      org-directory (expand-file-name "~/Dropbox/org/")
      org-default-notes-file (concat org-directory "/capture.org")
      org-refile-targets '((org-agenda-files :level . 1))
      )

