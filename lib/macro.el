;;; macro.el --- DWIM macro recording and playbac

;; Author: ??
;; Keywords: convenience
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?MacroKey

;;; Commentary:

;; This unifies macro recording and playback.  Use it by binding it to
;; a key.  Example setup in your ~/.emacs file:
;;
;; (global-set-key (kbd "<f9>") 'macro-dwim)
;; (global-set-key (kbd "S-<f9>") 'macro-clear)
;; (autoload 'macro-dwim "macro" "DWIM macro recording and playback." t)
;; (autoload 'macro-clear "macro "Clear last keyboard macro" t)

;;; Code:
(defun macro-dwim (arg)
  "DWIM keyboard macro recording and executing."
  (interactive "P")
  (if defining-kbd-macro
      (if arg
	  (end-kbd-macro arg)
	(end-kbd-macro))
    (if last-kbd-macro
	(call-last-kbd-macro arg)
      (start-kbd-macro arg))))

(defun macro-clear ()
  "Clear out the last keyboard macro."
  (interactive)
  (setq last-kbd-macro nil)
  (message "Last keyboard macro cleared."))
