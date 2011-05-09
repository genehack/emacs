;; run with emacs -batch rebuild-elc.el  -kill

(defvar genehack/rebuild-elc-el-files-list
  '(
    "lib/smart-tab/smart-tab.el"
    "lib/anything.el"
    "lib/anything-config.el"
    "lib/anything-match-plugin.el"
    "lib/cperl-mode/cperl-mode.el"
    "lib/js2-mode/js2-mode.el"
    )
  "list of files to rebuild." )

(cd "~/proj/emacs")

(add-to-list 'load-path (expand-file-name "./lib"))

(dolist (file genehack/rebuild-elc-el-files-list)
  (byte-compile-file file))
