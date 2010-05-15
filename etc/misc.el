;; misc.el -- various customizations and additions
;;; This is for stuff that _isn't_ built-in to Emacs

;;; AUTO COMPLETE
(add-to-list 'load-path (concat genehack/emacs-libs-dir "auto-complete"))
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (concat genehack/emacs-libs-dir "auto-complete/ac-dict"))
(ac-config-default)
(setq ac-comphist-file (concat genehack/emacs-config-dir "tmp/ac-comphist.dat"))
