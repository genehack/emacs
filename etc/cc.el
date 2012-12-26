;; cc.el - C/C++ specific customizations and code

(add-hook 'c-mode-common-hook 'linum-mode)

(require 'cc-mode)
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)
