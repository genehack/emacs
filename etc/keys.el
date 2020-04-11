;;; keys --- global key bindings
;;; Commentary:
;;; put all global key bindings here (so as to avoid binding
;;; conflicts)

;;; Code:

;;; GLOBAL ONLY -- mode-specific in mode-specific files
;;;; by default this is bound to tmm-menubar
(global-unset-key (kbd "M-`"))

(global-set-key (kbd "C-c o")      #'open-with)
(global-set-key (kbd "C-c C-SPC")  #'just-one-space)
(global-set-key (kbd "C-M-a")      #'sp-beginning-of-sexp)
(global-set-key (kbd "C-M-b")      #'sp-backward-sexp)
(global-set-key (kbd "C-M-e")      #'sp-end-of-sexp)
(global-set-key (kbd "C-M-f")      #'sp-forward-sexp)
(global-set-key (kbd "C-M-n")      #'sp-next-sexp)
(global-set-key (kbd "C-M-p")      #'sp-previous-sexp)
(global-set-key (kbd "C-S-RET")    #'open-line-above)
(global-set-key (kbd "C-RET")      #'open-line-below)
(global-set-key (kbd "C-%")        #'genehack/paren-bounce)
(global-set-key (kbd "C-5")        #'genehack/paren-bounce)
(global-set-key (kbd "C-=")        #'er/expand-region)
(global-set-key (kbd "C-#")        #'comment-or-uncomment-region)
(global-set-key (kbd "C-3")        #'comment-or-uncomment-region)
(global-set-key (kbd "C-a")        #'genehack/bol-toggle)
(global-set-key (kbd "C-c d")      #'dired-jump)
(global-set-key (kbd "C-c g")      #'counsel-git)
(global-set-key (kbd "C-c h")      #'helpful-at-point)
(global-set-key (kbd "C-c j")      #'counsel-git-grep)
(global-set-key (kbd "C-c k")      #'counsel-ag)
(global-set-key (kbd "C-c l")      #'genehack/git-blame-for-line)
(global-set-key (kbd "C-c C-A")    #'ag-regexp-project-at-point)
(global-set-key (kbd "C-c C-a")    #'ag-project)
(global-set-key (kbd "C-c C-b")    #'bury-buffer)
(global-set-key (kbd "C-c C-d")    #'genehack/dired-right-here)
(global-set-key (kbd "C-c C-g")    #'genehack/magit-key)
(global-set-key (kbd "C-c C-h")    #'genehack/split-horizontally-or-delete-other-windows)
(global-set-key (kbd "C-c C-l")    #'github-browse-file)
(global-set-key (kbd "C-c C-m")    #'genehack/perl-find-file-at-point)
(global-set-key (kbd "C-c C-o")    #'projectile-multi-occur)
(global-set-key (kbd "C-c C-p")    #'cperl-perldoc-at-point)
(global-set-key (kbd "C-c C-r")    #'ivy-resume)
(global-set-key (kbd "C-c C-t")    #'genehack/strip-whitespace-and-indent)
(global-set-key (kbd "C-c C-u")    #'genehack/unicode)
(global-set-key (kbd "C-c C-v")    #'genehack/split-vertically-or-delete-other-windows)
(global-set-key (kbd "C-c C-x")    #'maximize-frame)
(global-set-key (kbd "C-c M-x")    #'execute-extended-command)
(global-set-key (kbd "C-h f")      #'counsel-describe-function)
(global-set-key (kbd "C-h v")      #'counsel-describe-variable)
(global-set-key (kbd "C-x =")      #'genehack/diff-current-buffer-with-file)
(global-set-key (kbd "C-x b")      #'ivy-switch-buffer)
(global-set-key (kbd "C-x f")      #'genehack/find-file)
(global-set-key (kbd "C-x K")      #'genehack/kill-this-buffer)
(global-set-key (kbd "C-x p")      #'projectile-switch-project)
(global-set-key (kbd "C-x C-b")    #'ivy-switch-buffer)
(global-set-key (kbd "C-x C-f")    #'genehack/find-file)
(global-set-key (kbd "C-x M-b")    #'ibuffer)
(global-set-key (kbd "M-SPC")      #'genehack/kill-space-forward)
(global-set-key (kbd "M-+")        #'genehack/global-text-scale-increase)
(global-set-key (kbd "M-=")        #'text-scale-increase)
(global-set-key (kbd "M-_")        #'genehack/global-text-scale-decrease)
(global-set-key (kbd "M--")        #'text-scale-decrease)
(global-set-key (kbd "M-0")        #'genehack/text-scale-default)
(global-set-key (kbd "M-)")        #'genehack/global-text-scale-reset)
(global-set-key (kbd "M-`")        #'other-window)
(global-set-key (kbd "M-g")        #'goto-line)
(global-set-key (kbd "M-p")        #'genehack/print-buffer)
(global-set-key (kbd "M-s")        #'imenu-goto-symbol)
(global-set-key (kbd "M-x")        #'counsel-M-x)
(global-set-key (kbd "RET")        #'reindent-then-newline-and-indent)

;;; F keys
;; the things bound to genehack/noop are shadowed by the hardwired
;; multimedia keys on my keyboard -- but sometimes i use the keyboard
;; on my laptop, and i don't want something to happen if i
;; accidentally hit one of those keys there. i *could* just unbind
;; them, but then some other code might think they're available -- and
;; they're not, they're all MINE!!! BWAHAHAHAH.
;;
;; ahem.
(global-set-key (kbd "<f1>")      #'disk)
(global-set-key (kbd "<f2>")      #'genehack/save-and-kill)
;;;; unset these so they can be prefix keys
(global-unset-key (kbd "<f3>"))
(global-unset-key (kbd "<f4>"))
(global-set-key (kbd "<f3> f")    #'counsel-describe-function)
(global-set-key (kbd "<f3> v")    #'counsel-describe-variable)
(global-set-key (kbd "<f3> l")    #'counsel-find-library)
(global-set-key (kbd "<f3> i")    #'counsel-info-lookup-symbol)
(global-set-key (kbd "<f4> u")    #'counsel-unicode-char)
(global-set-key (kbd "<f5>")      #'genehack/noop)
(global-set-key (kbd "<f6>")      #'genehack/macro-dwim)
(global-set-key (kbd "<f7>")      #'genehack/macro-clear)
(global-set-key (kbd "<f8>")      #'multi-term-dedicated-toggle)
(global-set-key (kbd "<f9>")      #'genehack/noop)
(global-set-key (kbd "<f10>")     #'genehack/noop)
(global-set-key (kbd "<f11>")     #'genehack/noop)
(global-set-key (kbd "<f12>")     #'flycheck-next-error)
(global-set-key (kbd "C-<f12>")   #'flycheck-next-error)

(provide 'keys)
;;; keys.el ends here
