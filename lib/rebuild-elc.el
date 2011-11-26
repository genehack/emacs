;; run with emacs -batch rebuild-elc.el  -kill

(defvar genehack/rebuild-elc-el-files-list
  '(
    "lib/auto-complete/auto-complete-config.el"
    "lib/auto-complete/auto-complete.el"
    "lib/auto-complete/fuzzy.el"
    "lib/auto-complete/popup.el"
    "lib/anything/anything-config.el"
    "lib/anything/anything-match-plugin.el"
    "lib/anything/anything.el"
    "lib/cperl-mode/cperl-mode.el"
    "lib/delim-kill/delim-kill.el"
    "lib/flymake/flymake.el"
    "lib/flymake-perlcritic/flymake-perlcritic.el"
    "lib/js2-mode/js2-mode.el"
    "lib/smart-tab/smart-tab.el"
    )
  "list of files to rebuild." )

(cd "~/proj/emacs")

(add-to-list 'load-path (expand-file-name "./lib"))

(dolist (file genehack/rebuild-elc-el-files-list)
  (byte-compile-file file))
