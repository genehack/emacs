;; run with emacs -batch rebuild-elc.el  -kill

(defvar genehack/rebuild-elc-el-files-list
  '(
    "lib/anything/anything-config.el"
    "lib/anything/anything-match-plugin.el"
    "lib/anything/anything.el"
    "lib/coffee-mode/coffee-mode.el"
    "lib/cperl-mode/cperl-mode.el"
    "lib/js2-mode/js2-mode.el"
    "lib/smart-tab/smart-tab.el"
    "lib/textmate/textmate.el"
    )
  "list of files to rebuild." )

(cd "~/proj/emacs")

(add-to-list 'load-path (expand-file-name "./lib"))

(dolist (file genehack/rebuild-elc-el-files-list)
  (byte-compile-file file))
