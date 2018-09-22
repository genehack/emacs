;;; cc.el --- C/C++ specific customizations and code

;;; Commentary:

;;; Code:
(require 'use-package)

(use-package cc-mode
  :ensure t
  :defer t
  :config
  ;; display-line-numbers is Emacs 26 specific; if this
  ;; gives you problems, conditionalize between it and
  ;; linum-mode
  (add-hook 'c-mode-common-hook 'display-line-numbers-mode)
  (define-key c-mode-base-map (kbd "RET") 'newline-and-indent))

(defun my-csharp-mode-setup ()
  "Set up csharp mode for me."
  (omnisharp-mode)
  (company-mode)
  (flycheck-mode)

  (setq indent-tabs-mode nil)
  (setq c-syntactic-indentation t)

  (local-set-key (kbd "C-c C-c") 'recompile))

(add-hook 'csharp-mode-hook 'my-csharp-mode-setup t)


(provide 'cc)
;;; cc.el ends here
