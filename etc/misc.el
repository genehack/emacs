;; misc.el -- various customizations and additions
;;; This is for stuff that _isn't_ built-in to Emacs

;;; AUTO COMPLETE
(add-to-list 'load-path (concat genehack/emacs-libs-dir "auto-complete"))
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (concat genehack/emacs-libs-dir "auto-complete/ac-dict"))
(ac-config-default)
(setq ac-comphist-file (concat genehack/emacs-config-dir "tmp/ac-comphist.dat"))

;;; AUTO CREATE DIRECTORIES
;;;; after <http://atomized.org/2008/12/emacs-create-directory-before-saving/>
(add-hook 'before-save-hook
          '(lambda ()
             (or (file-exists-p (file-name-directory buffer-file-name))
                 (make-directory (file-name-directory buffer-file-name) t))))

;;; BROWSE-KILL-RING
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;;; COLOR-THEME
(add-to-list 'load-path (concat genehack/emacs-libs-dir "color-theme"))
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-hober)))

;;; CSS-HEXCOLOR
(require 'css-hexcolor)

;;; DIFF-CURRENT-BUFFER-WITH-FILE
(defun genehack/diff-current-buffer-with-file ()
  "Show diff between current buffer contents and file on disk"
  (interactive)
  (diff-buffer-with-file (current-buffer)))

;;; DIRED-RIGHT-HERE
(defun genehack/dired-right-here (arg)
  "Run ido-dired or, with prefix, dired on current active directory."
  (interactive "p")
  (if (eq 1 arg)
      (ido-dired)
    (dired default-directory)))

;;; DISK
(autoload 'disk "disk" "Save, revert, or find file." t)

;;; FILLADAPT -- WTF isn't this part of emacs by default by now?!
(require 'filladapt)

;;; FIXME
(require 'fixme)

;;; FLYMAKE FOR CSS FILES
;;;; after <http://www.emacswiki.org/emacs-en/FlymakeCSS>
(if (file-exists-p "/opt/css-validator")
    (progn
      (require 'flymake)
      (defconst css-validator "java -jar /opt/css-validator/css-validator.jar")

      (defun flymake-css-init ()
        (let* ((temp-file   (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))
               (local-file  (file-relative-name
                             temp-file
                             (file-name-directory buffer-file-name))))
          (list "java" (list "-jar" "/opt/css-validator/css-validator.jar" "-output" "gnu" (concat "file:" local-file)))))

      (push '(".+\\.css$" flymake-css-init) flymake-allowed-file-name-masks)
      (push '("^file:\\([^:]+\\):\\([^:]+\\):\\(.*\\)" 1 2 nil 3) flymake-err-line-patterns)

      (add-hook 'css-mode-hook 'flymake-mode)))
