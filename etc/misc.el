;; misc.el -- various customizations and additions
;;; This is for stuff that _isn't_ built-in to Emacs

;;; AUTO COMPLETE
(add-to-list 'load-path (concat genehack/emacs-libs-dir "auto-complete"))
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (concat genehack/emacs-libs-dir "auto-complete/ac-dict"))
(ac-config-default)
(setq ac-comphist-file (concat genehack/emacs-config-dir "tmp/ac-comphist.dat"))

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
