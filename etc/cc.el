;;; cc.el --- C/C++ specific customizations and code

;;; Commentary:

;;; Code:
(require 'use-package)

(use-package cc-mode
  :config
  ;; display-line-numbers is Emacs 26 specific; if this
  ;; gives you problems, conditionalize between it and
  ;; linum-mode
  (add-hook 'c-mode-common-hook 'display-line-numbers-mode)
  (define-key c-mode-base-map (kbd "RET") 'newline-and-indent))

(provide 'cc)
;;; cc.el ends here
