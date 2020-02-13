;; init.el -- master configuration file

;;; Commentary:

;;; Code:
;; VARIABLES
;;; if you're not me, you probably want to change this to point to
;;; where you put your checkout of the github repo:
(defvar genehack/emacs-dir (expand-file-name "~/proj/emacs/")
  "Directory containing working copy of Emacs config repo.")

(defvar genehack/emacs-config-dir (concat genehack/emacs-dir "etc/")
  "Sub-directory containing config files.")
(add-to-list 'load-path genehack/emacs-config-dir)

(defvar genehack/emacs-lib-dir (concat genehack/emacs-dir "lib/")
  "Sub-directory containing non-(M)ELPA library files.")
(add-to-list 'load-path genehack/emacs-lib-dir)

(defvar genehack/emacs-tmp-dir (concat genehack/emacs-dir "tmp/")
  "Scratch space for stuff...")

;;; I generally don't use the Customize interface, but sometimes
;;; things get added there. Setting this means the file is under
;;; revision control, so if something touches it, I'll notice (and
;;; then I can move stuff to where it really goes and empty out
;;; custom.el again...)
(setq custom-file (concat genehack/emacs-config-dir "custom.el"))
(load custom-file)

;; DEFAULT FACE
;;; If you don't set this early on, sometimes things get wonky.
(if (eq system-type 'darwin)
    (set-face-attribute 'default t
                        :background "#000000"
                        :foreground "#ffffff"
                        :family "Fira Code"
                        :height 180)
  (set-face-attribute 'default t
                      :background "#000000"
                      :foreground "#ffffff"
                      :family "Mono"
                      :height 161))

;; ELPA
(require 'package)
(setq package-user-dir (concat genehack/emacs-dir "elpa"))
;;; initialize ELPA, creating package directory if necessary
;;; (and complaining if we're blocked by a file...)
(if (file-exists-p package-user-dir)
    (if (file-directory-p package-user-dir)
        (package-initialize)
      (error "ELPA package dir creation blocked by file"))
  (make-directory package-user-dir))

;; PRIVATE CONFIG
;;; Again, you might want to change this path to a private file of
;;; your own. This is a place to put variables that set passwords,
;;; email addresses, and other stuff that you don't want in a public
;;; github repo...
(defvar genehack/private-config-file "~/private/emacs-private.el"
  "File with configuration info that can't be in public repository.")
(if (file-readable-p genehack/private-config-file)
    (progn
      (load-library genehack/private-config-file)
      (message "Loaded private config")))

;; PACKAGES
(eval-after-load "package"
  '(setq package-archives
         '(("my-melpa" . "http://melpa.genehack.net/packages/")
           ("melpa"    . "http://melpa.org/packages/")
           ("gnu" . "http://elpa.gnu.org/packages/"))))

(defvar genehack/packages-refreshed nil
  "Flag for whether package lists have been refreshed yet.")

(defadvice package-install (before refresh activate)
  "Call `package-refresh-contents` once before `package-install`."
  (unless (eq genehack/packages-refreshed t)
    (progn
      (package-refresh-contents)
      (setq genehack/packages-refreshed t))))

;;; we need use-package -- it'll take care of installing everything
;;; else
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;; GARBAGE COLLECTION MAGIC HACK
;;; speeds startup?
(use-package gcmh
  :ensure t
  :init (gcmh-mode 1))

;; MAKE EMACS PATH MATCH SHELL PATH
(setenv "PLENV_ROOT" "/opt/plenv")
(use-package exec-path-from-shell
  :ensure exec-path-from-shell
  :init
  (setq exec-path-from-shell-check-startup-files nil
        exec-path-from-shell-variables
        '("PLENV_ROOT" "PATH" "MANPATH" "GOROOT" "GOPATH" "NVM_DIR" "SSH_AUTH_SOCK" "SSH_AGENT_PID"))
  (exec-path-from-shell-initialize))

;; MODULES
;;; All the rest of the config is split out into individual files, for
;;; ease of use.
(defvar genehack/module-list
  '(
    "builtins"
    "cc"
    "completion"
    "keys"
    "misc"
    "perl"
    )
  "List of modules to load on startup.")

(dolist (pkg genehack/module-list)
   (if (file-readable-p (concat genehack/emacs-config-dir pkg ".el"))
       (load-library pkg)))

(provide 'init)
;;; init.el ends here
