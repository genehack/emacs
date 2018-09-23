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
  :defer t
  :diminish (company-mode . " Co")
  :bind
  ("\t" . genehack/company-yasnippet-or-completion)
  :config
  (setq company-echo-delay 0
        company-idle-delay 0.3
        company-minimum-prefix-length 1
        company-show-numbers t
        company-tooltip-align-annotations 't)
  (setq-default company-backends
                '(company-web-html
                  company-css
                  company-lsp
                  company-go
                  company-nxml
                  company-files
                  (company-capf :with company-yasnippet)
                  (company-dabbrev-code company-keywords)
                  company-dabbrev))
  :init (global-company-mode))

(use-package company-box
  :ensure t
  :after company
  :diminish (company-box-mode . " cb")
  :hook (company-mode . company-box-mode))

;;;; depends on go-mode, so put this down here...
(use-package company-go
  :after go-mode company
  :defer t
  :ensure t)

(use-package company-web
  :ensure t
  :after company
  )

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
  :defer t
  :diminish (lsp-mode . " lsp"))

(use-package lsp-javascript-typescript
  :defer t
  :ensure t)

(use-package lsp-ui
  :after lsp-mode
  :defer t
  :ensure t
  :hook (lsp-mode . lsp-ui-mode))

;;; OMNISHARP
(use-package omnisharp
  :after company
  :ensure t
  :defer t
  :hook (csharp-mode . omnisharp-mode)
  :config
  (add-to-list 'company-backends 'company-omnisharp))

;;; SMART-TAB
(use-package smart-tab
  :ensure t
  :defer t
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

(provide 'completion)
;;; completion.el ends here
