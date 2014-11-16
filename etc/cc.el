;;; cc.el --- C/C++ specific customizations and code

;;; Commentary:

;;; Code:
(require 'use-package)
(use-package cc-mode
  :config (progn
            (add-hook 'c-mode-common-hook 'linum-mode)
            (define-key c-mode-base-map (kbd "RET") 'newline-and-indent)))

(provide 'cc)
;;; cc.el ends here
