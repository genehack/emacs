;; completion.el --- auto-complete related config
;;; Commentary:
;;;

;;; Code:

;;; COMPANY-MODE
;; inspired by https://gist.github.com/nonsequitur/265010
(require 'company)
(defun genehack/company-yasnippet-or-completion ()
  "Expand yasnippet if available, otherwise autocomplete."
  (interactive)
  (if (first (yas--templates-for-key-at-point))
      (progn (company-abort)
             (yas-expand))
    (company-complete)))

(use-package company
  :ensure t
  :diminish (company-mode . " Co")
  :bind
  ("\t" . genehack/company-yasnippet-or-completion)
  :config
  (setq company-echo-delay 0
        company-idle-delay 0.3
        company-minimum-prefix-length 1)
  (setq-default company-backends
                '(company-lsp
                  company-go
                  company-omnisharp
                  company-nxml
                  ;; company-ycmd
                  company-html
                  company-css
                  company-files
                  (company-capf :with company-yasnippet)
                  (company-dabbrev-code company-keywords)
                  company-dabbrev))
  :init (global-company-mode))

(setq company-backend '(company-lsp))

;;;; depends on go-mode, so put this down here...
(use-package company-go
  :after go-mode
  :ensure t)


;;; HIPPY-EXPAND
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))


;;; LSP
(use-package lsp-mode
  :ensure t
  :diminish (lsp-mode . " lsp")
  :config
  )
(use-package lsp-ui
  :ensure t
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))
(use-package lsp-javascript-typescript :ensure t)

;;; SMART-TAB
(use-package smart-tab
  :ensure t
  :diminish (smart-tab-mode . " st")
  :config
  (setq smart-tab-using-hippie-expand t)
  (setq smart-tab-user-provided-completion-function 'company-complete)
  (setq smart-tab-completion-functions-alist
        '((cperl-mode      . genehack/company-yasnippet-or-completion)
          (emacs-lisp-mode . genehack/company-yasnippet-or-completion)
          (js2-mode        . genehack/company-yasnippet-or-completion)
          (lisp-mode       . genehack/company-yasnippet-or-completion)
          (go-mode         . genehack/company-yasnippet-or-completion)
          (text-mode       . dabbrev-completion)))
  (global-smart-tab-mode 1))

;;; YCMD
;; (use-package ycmd
;;   :config
;;   (add-hook 'after-init-hook #'global-ycmd-mode)
;;   (set-variable 'ycmd-server-command `("python" ,(file-truename "~/src/ycmd/ycmd")))
;;   :ensure t)

;; (use-package company-ycmd
;;   :init
;;   (company-ycmd-setup)
;;   :ensure t)

;; (use-package flycheck-ycmd
;;   :init
;;   (flycheck-ycmd-setup)
;;   :ensure t)



(provide 'completion)
;;; completion.el ends here
