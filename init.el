;; init.el -- master configuration file

;; VARIABLES
;;; if you're not me, you probably want to change this to point to
;;; where you put your checkout of the github repo:
(defvar genehack/emacs-dir (expand-file-name "~/proj/emacs/")
  "directory containing working copy of 'emacs' repo")

(defvar genehack/emacs-config-dir (concat genehack/emacs-dir "etc/")
  "sub-directory containing config files")
(add-to-list 'load-path genehack/emacs-config-dir)

(defvar genehack/emacs-libs-dir (concat genehack/emacs-dir "lib/")
  "sub-directory containing third-party emacs libs")
(add-to-list 'load-path genehack/emacs-libs-dir)

(defvar genehack/emacs-tmp-dir (concat genehack/emacs-dir "tmp/")
  "scratch space for stuff...")

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

;; CL
;;; most everything uses this so let's just get it out of the way...
(require 'cl)

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
  "file with configuration info that can't be in public repository.")
(if (file-readable-p genehack/private-config-file)
    (load-library genehack/private-config-file))

;; HELPER FUNCTIONS
;;; This is here because it's used across several different config
;;; files as a helper function and it needs to be defined before we
;;; try to load those files.
(defun genehack/add-emacs-lib-subdir-to-load-path (dir)
  "Concat arg with genehack/emacs-libs-dir and add to load-path"
  (add-to-list 'load-path (concat genehack/emacs-libs-dir dir)))

;; PACKAGES
;;; if it's not in my melpa, i don't care.
(eval-after-load "package" '(setq package-archives '(("melpa" . "http://melpa.genehack.net/packages/"))))

(defvar genehack/package-list
  '(
    ac-nrepl
    ag
    annoying-arrows-mode
    auto-complete
    autopair
    browse-kill-ring
    clojure-mode
    clojure-snippets
    clojure-test-mode
    coffee-mode
    cperl-mode
    css-mode
    delim-kill
    diminish
    dired+
    dired-details
    dired-details+
    disk
    exec-path-from-shell
    expand-region
    find-file-in-project
    flycheck
    flycheck-color-mode-line
    flymake
    flymake-cursor
    flymake-perlcritic
    flymake-shell
    full-ack
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
    ido-hacks
    ido-ubiquitous
    js2-mode
    kolon-mode
    magit
    markdown-mode
    markdown-mode+
    move-text
    multi-term
    nrepl
    paredit
    perlcritic
    powerline
    pretty-lambdada
    projectile
    scala-mode2
    smart-tab
    smex
    solarized-theme
    tidy
    twilight-theme
    unbound
    web-mode
    yaml-mode
    yasnippet
    zenburn-theme
    ) "list of packages to automatically install" )

;; prevent long installs from borking overall process
(setq url-http-attempt-keepalives nil)

(defvar genehack/packages-refreshed nil
  "flag for whether package lists have been refreshed yet")

;; install anything that's missing
(dolist (pkg genehack/package-list)
  (if (not (package-installed-p pkg))
      (progn
        (if (not (eq genehack/packages-refreshed t))
            (progn
              (package-refresh-contents)
              (setq genehack/packages-refreshed t)))
        (package-install pkg))))

;; MAKE EMACS PATH MATCH SHELL PATH
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
  "list of modules to load on startup.")

(dolist (pkg genehack/module-list)
  (if (file-readable-p (concat genehack/emacs-config-dir pkg ".el"))
      (load-library pkg)))
