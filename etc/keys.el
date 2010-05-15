;; keys.el
;;; put all global key bindings here (so as to avoid binding
;;; conflicts)

;; GLOBAL ONLY -- mode-specific in mode-specific files
(global-set-key (kbd "C-x =") 'genehack/diff-current-buffer-with-file)
(global-set-key (kbd "C-%")   'genehack/paren-bounce)
(global-set-key (kbd "C-a")   'genehack/bol-toggle)

;;; F keys
(global-set-key (kbd "<f1>")      'disk)
(global-set-key (kbd "<f2>")      'genehack/dired-right-here)
(global-set-key (kbd "<f3>")      'genehack/magit-status-with-prompt)

(global-set-key (kbd "<f11>")     'multi-term-dedicated-toggle)
(global-set-key (kbd "<f12>")     'macro-dwim)
(global-set-key (kbd "M-<f12>")   'macro-clear)
(global-set-key (kbd "ESC <f12>") 'macro-clear)
