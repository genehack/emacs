(genehack/add-emacs-lib-subdir-to-load-path "org/lisp")
(genehack/add-emacs-lib-subdir-to-load-path "org/contrib/lisp")
(require 'org-install)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(setq org-agenda-files '("~/Dropbox/org/"
                         "~/Dropbox/org/home/"
                         "~/Dropbox/org/work/")
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-start-on-weekday 0
      org-blank-before-new-entry
      '((heading . t) (plain-list-item . nil))
      org-directory (expand-file-name "~/Dropbox/org/")
      org-default-notes-file (concat org-directory "/capture.org")
      org-log-done t
      org-mobile-inbox-for-pull "~/Dropbox/org/capture.org"
      org-mobile-directory "~/Dropbox/MobileOrg"
      org-refile-targets '((org-agenda-files :maxlevel . 2))
      org-refile-use-outline-path 'file
      org-return-follows-link t
      org-src-fontify-natively t
      org-startup-folded nil
      org-startup-indented 1
      org-todo-keywords '((sequence "TODO(t!)" "NEXT(n!)" "|" "DONE(!)")
                          (sequence "WAITING(w!)" "|" "DONE(!)" ))
      org-M-RET-may-split-line '((headline nil)(item nil)(default t))
      )


(defun genehack/open-org-capture-file ()
  "Open the default org file to capture something."
  (interactive)
  (find-file org-default-notes-file))


(defun genehack/kill-all-org-buffers ()
  "Kill all open buffers that have names ending in '.org'."
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (let ((name (buffer-name)))
        (if (string-match "\\.org$" name)
            (progn
              (save-buffer)
              (kill-buffer buffer)))))))

(defun genehack/insert-org-date (&optional prefix)
  "Insert current day in org-timestamp format
Or, with prefix, prompt for day to insert."
  (interactive "p")
  (if (eq prefix 1)
      (org-insert-time-stamp (current-time))
    (org-time-stamp nil)))
(define-key org-mode-map (kbd "C-c .") 'genehack/insert-org-date)
