;; keys.el
;;; put all global key bindings here (so as to avoid binding
;;; conflicts)

;; GLOBAL ONLY -- mode-specific in mode-specific files
(global-set-key (kbd "C-x =") 'genehack/diff-current-buffer-with-file)

;;; F keys
(global-set-key (kbd "<f2>") 'genehack/dired-right-here)

