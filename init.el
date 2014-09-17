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

(defvar genehack/emacs-tmp-dir (concat genehack/emacs-dir "tmp/")
  "Scratch space for stuff...")

;;; I generally don't use the Customize interface, but sometimes
;;; things get added there. Setting this means the file is under
;;; revision control, so if something touches it, I'll notice.
(setq custom-file (concat genehack/emacs-config-dir "custom.el"))

;; DEFAULT FACE
;;; If you don't set this early on, sometimes things get wonky.
(if (eq system-type 'darwin)
    (set-face-attribute 'default t
                        :background "#000000"
                        :foreground "#ffffff"
                        :family "Source Code Pro"
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
    (load-library genehack/private-config-file))

;; PACKAGES
;;; if it's not in my melpa, i don't care.
(eval-after-load "package" '(setq package-archives '(("melpa" . "http://melpa.genehack.net/packages/"))))

(defvar genehack/package-list
  '(
    ag
    annoying-arrows-mode
    autopair
    browse-kill-ring
    clojure-mode
    clojure-snippets
    company
    company-go
    cperl-mode
    css-mode
    diminish
    dired+
    dired-details
    dired-details+
    disk
    exec-path-from-shell
    expand-region
    find-file-in-project
    fiplr
    flycheck
    flycheck-color-mode-line
    flymake-cursor
    flymake-easy
    flymake-perlcritic
    flymake-shell
    genehack-misc-elisp
    genehack-perl-elisp
    gh
    gist
    git-blame
    git-commit-mode
    git-gutter
    gitconfig-mode
    github-browse-file
    gitignore-mode
    go-autocomplete
    go-direx
    go-eldoc
    go-errcheck
    go-mode
    go-snippets
    helm
    helm-projectile
    js2-mode
    kolon-mode
    magit
    markdown-mode
    markdown-mode+
    maxframe
    move-text
    multi-term
    perlcritic
    pretty-lambdada
    projectile
    scala-mode2
    smart-tab
    smartparens
    smex
    solarized-theme
    tidy
    twilight-theme
    unbound
    web-mode
    yaml-mode
    yasnippet
    zenburn-theme
    ) "List of packages to automatically install." )

(defvar genehack/packages-to-warn-about
  '(
    coffee-mode
    delim-kill
    full-ack
    ido-hacks
    ido-ubiquitous
    powerline
    ) "List of packages that should not be installed.  If seen, will cause warning." )

(defvar genehack/packages-refreshed nil
  "Flag for whether package lists have been refreshed yet.")

;;; install anything that's missing
(dolist (pkg genehack/package-list)
  (if (not (package-installed-p pkg))
      (progn
        (if (not (eq genehack/packages-refreshed t))
            (progn
              (package-refresh-contents)
              (setq genehack/packages-refreshed t)))
        (package-install pkg))))

;;; and warn about stuff that shouldn't be there
(dolist (pkg genehack/packages-to-warn-about)
  (if (package-installed-p pkg)
      (warn "Package %s installed, please remove" pkg)))

;; MAKE EMACS PATH MATCH SHELL PATH
(require 'exec-path-from-shell)
(setq exec-path-from-shell-variables '("PATH" "MANPATH" "GOROOT" "GOPATH"))
(exec-path-from-shell-initialize)


;; MODULES
;;; All the rest of the config is split out into individual files, for
;;; ease of use.
(defvar genehack/module-list
  '(
    "builtins"
    "cc"
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
