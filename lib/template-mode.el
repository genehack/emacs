;;; template-mode.el --- Emacs major and minor modes for editing Template-Toolkit files

;; Copyright (C) 2007, 2008, 2009 Andreas Spindler.  Maintained at <http://www.visualco.de>.

;; Author: Andreas Spindler
;; Keywords: Emacs, Perl, HTML, Template toolkit, TT, TT2

;; This file is  free software; you can redistribute it  and/or modify it under
;; the  terms of  the  GNU General  Public  License as  published  by the  Free
;; Software  Foundation;  either version  2,  or  (at  your option)  any  later
;; version.

;; This file is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without  even the implied  warranty of MERCHANTABILITY  or FITNESS
;; FOR  A PARTICULAR  PURPOSE.  See  the GNU  General Public  License  for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; GNU  Emacs; see  the  file COPYING.   If  not, write  to  the Free  Software
;; Foundation, Inc.,  51 Franklin Street,  Fifth Floor, Boston,  MA 02110-1301,
;; USA.

;;; Commentary:

;; Provides  extra  fontifying for  text  files to  be  processed  by the  Perl
;; Template Toolkit.  New mode functions provided by this package:
;;		`template-mode'
;;		`minor-template-mode'
;;		`outline-template-mode'
;; which will fontify embedded placeholder  tags.  

;; The Perl  toolkit process any text  files by expanding embedded  tags of the
;; form
;;;		[% code %]
;; where "code" is  evaluated, and the the whole tag is  replaced by the result
;; of the  evaluation.  For example, this  is very convenient  for Perl scripts
;; that generate  HTML, since  the scripts must  not contain  any HTML-snippets
;; anymore.  See <http://search.cpan.org/perldoc?Template::Toolkit>.

;;; Portability:

;; This mode use shy-grouping of regular expressions. This file has been tested
;; with Emacs 21+. It hasn't been tested with Emacs 20, nor XEmacs.

;;; Related modes:

;; Another Template-Toolkit mode is <tt-mode.el> by Dave Cross. The development
;; seems to have stopped in 2002. <tt-mode.el> defines only a major mode, so in
;; the first place buffers are  in `fundamental-mode' or `text-mode'.  Also '['
;; and ']' are not electric.  The  discontent with this mode was the motivation
;; for <template-mode.el>.

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'regexp-opt)
  (require 'font-lock))

(defvar template-minor-mode nil
  "Template Toolkit minor mode.")

;;; -------- KEYMAP
;; ----------------------------------------------------------------------------

;; The keymap for `template-minor-mode'.  Enable keys useful for text files.
;; It also makes [ and ] characters electric, which is especially nice when the
;; dominant mode is `html-mode'.

;; TODO: `template-mode-mark-tag' function that marks the placeholder point is
;; on.

(defvar template-mode-map nil)
(unless template-mode-map
  (setq template-mode-map (make-sparse-keymap))
  (define-key template-mode-map "\C-m" 'newline-and-indent)
  (define-key template-mode-map "\C-l" 'recenter)
  (define-key template-mode-map "[" 'template-mode-electric-brace)
  (define-key template-mode-map "]" 'template-mode-electric-brace)
  )

(defun template-mode-electric-brace (arg)
  "Insert the key typed and maybe correct line's indentation."
  (interactive "P")
  (self-insert-command (prefix-numeric-value arg))
  ;; Indent unless at end of line; the regexp functions better than `eolp'.
  (if (not (looking-at "[ \t]*\\\\?$"))
	  (indent-according-to-mode)))

;;; -------- FONT-LOCKING
;; ----------------------------------------------------------------------------

;; Font-locking implementation.  To fontify the placeholder expressions for the
;; Template Toolkit is the main concern of <template-mode.el>.
;;
;; `template-mode-flatten', `template-mode-join' and `template-mode-regexp-...'
;; are private utility functions used to build regular expressions.  [Note: the
;; `regexp-opt' is better suited, since it is dedicated for this task. But it
;; behaves differently among Emacsens, and may not even been available.]

;; The Template Toolkit lets you freely define the open/close tags. This is
;; currently not the case with <template-mode.el>, which only supports two
;; syntaxes:
;;		[% text %]
;;		[[ text ]]
;; TODO: provide a `template-mode-set-tag-style' function that rebuilds all
;; relevant regexes.
;;
;; Finally, all efforts flow into `template-mode-turn-on-font-lock', the
;; function called by the mode-enabling functions.

(defun template-mode-flatten (lst)
  "In LST undoes any nesting of sub-lists, causing all elements to be on the top-level. All removes
all `nil' elements.
See \"Writing GNU Emcas extensions\" by B. Glickstein, p. 86."
  (if (null lst) nil
    (if (listp (car lst))
        (append (template-mode-flatten (car lst))
                (template-mode-flatten (cdr lst)))
      (cons (car lst)
			(template-mode-flatten (cdr lst))))))
(defun template-mode-join (sep &rest args)
  "Like Perl's `join'."
  (mapconcat '(lambda(x)x) (template-mode-flatten args) sep))
(defun template-mode-regexp (strings &optional func)
  "Group Emacs regular expressions in the list STRINGS.
FUNC controls how strings are transformed and grouped.  Legal arguments are
nil (don't group), t (group), `shy' (shy group), `words' (match the empty
string before/after STRINGS), or `shy-words'."
  (if (listp strings)
	  (if (null strings) ""
		(save-match-data
		  (let ((shy (or (eq func 'shy)
						 (eq func 'shy-words)))
				(words (or (eq func 'words)
						   (eq func 'shy-words)))
				(sorted-strings
				 (template-mode-flatten (delete-dups (sort (copy-sequence strings) 'string-lessp)))))
			(if (= (length sorted-strings) 0) ""
			  (let ((open (if func (if shy "\\(?:" "\\(") ""))
					(close (if func "\\)" ""))
					(re (template-mode-join "\\|" sorted-strings)))
				(concat open (if words (concat "\\<\\(?:" re "\\)\\>") re) close)))
			)
		  )
		)
	(template-mode-regexp (list strings) func)))

(defun template-mode-tt2-regexp (strings &optional all)
  "Build regular expression matching a Template Toolkit tag.  
If ALL is nil `match-string' 1 will return the inner part."
  (if (listp strings)
	  (template-mode-regexp
	   (list (concat (template-mode-regexp template-mode-open-re all)
					 (template-mode-regexp strings t)
					 (template-mode-regexp template-mode-close-re all))))
	(template-mode-tt2-regexp (list strings) all)))

(defconst template-mode-open-re (template-mode-regexp (list "\\[%[-+]?\\s-*" "\\[\\[[-+]?\\s-*") 'shy))
(defconst template-mode-close-re (template-mode-regexp (list "\\s-*%\\]" "\\s-*\\]\\]") 'shy))
;; (defconst template-mode-open-re (template-mode-regexp "\\[%[-+]?\\s-*"))
;; (defconst template-mode-close-re (template-mode-regexp "\\s-*%\\]"))
(defconst template-mode-keyword-list
  '("block" "call" "catch" "case" "clear" "default" "end" "elsif" "else"
	"for" "foreach" "filter" "final" "get"
	"if" "insert" "include"
	"process" "last"
	"macro" "meta" "perl"
	"unless" "use" "wrapper" "while" "rawperl"
	"return" "stop" "switch" "set" "tags" "try" "throw")
  "List of Template Toolkit keywords.")

(defvar template-mode-face-alist
  '(("default" . default)
    ("bold" . bold)
    ("highlight" . highlight)
    ("emphasize" . italic)
    ("underline" . underline)
    ("modeline" . modeline)
    ("bu" bold underline)
    ("beu" bold-italic underline)
    ("eu" italic underline)
    ("small" . default)
    ("strong" . bold)))

(defvar template-mode-font-lock-1 
  (list
   ;; Fontify tagged expressions (tags) of the form
   ;;	[% text %]
   ;; or
   ;;	[[ text ]]
   ;; The latter I prefer; IMHO it is easier to type and to spot.
    (list (template-mode-tt2-regexp ".+?" t)
		  '(1 font-lock-builtin-face t)
		  '(2 font-lock-constant-face t)
		  '(3 font-lock-builtin-face t))
   ;; Based on the above, fontify keywords and comment within the tags.
	(list (template-mode-tt2-regexp "#.*?")
		  '(1 font-lock-comment-face t))
	(list (template-mode-tt2-regexp (list template-mode-keyword-list))
		  '(1 font-lock-keyword-face t)))
  "Expressions to font-lock in `template-minor-mode'.")

(defconst template-mode-font-lock-2
  (append
   template-mode-font-lock-1
    '((eval . (cons
			   ;;"\\(\\[% *[^#].*? *%\\]\\)"
			   ;;(template-mode-regexp (template-mode-tt2-regexp ".+?") t)
			   (template-mode-tt2-regexp ".+?")
			   '(1 (cdr (assoc "bold" template-mode-face-alist)) prepend))))
	)
  "Extend `template-mode-font-lock-1' so that all tags are rendered bold, and variables additionally underlined.")

(defun template-mode-turn-on-font-lock ()
  (turn-on-font-lock)
  (font-lock-add-keywords nil template-mode-font-lock-2)
  (font-lock-fontify-buffer))

;;; -------- INTERFACE TO ENABLE MODES
;; ----------------------------------------------------------------------------

;;;###autoload
(defun turn-on-template-mode ()
  "Unconditionally turn on Template-Toolkit minor mode."
  (interactive) (template-minor-mode 1))

;;;###autoload
(defun turn-off-template-mode ()
  "Unconditionally turn off Template-Toolkit minor mode."
  (interactive) (template-minor-mode 0))

;;;###autoload
(define-minor-mode template-minor-mode
  "Toggle Template-Toolkit minor-mode.
Basically `template-minor-mode' adds fontifying for tagged expressions the
Template Toolkit would replace."
  nil " Template" template-mode-map
  ;; Code to execute each time the mode is activated or deactivated, but before
  ;; the hook.
  (template-mode-turn-on-font-lock)
  (run-hooks 'template-mode-hook))

;;;###autoload
(defun outline-template-mode ()
  "Major mode for editing text files to be expanded by the Perl Template Toolkit.
This mode actually is a hybrid mode: it combines `outline-mode' and
`template-minor-mode'."
  (interactive)
  (outline-mode) (template-minor-mode))

;;;###autoload
(defun template-mode ()
  "Major mode for editing text files to be expanded by the Perl Template Toolkit.
This mode fontifies embedded tags; files are otherwise in `fundamental-mode' or
`text-mode'.  See also `outline-template-mode'."
  (kill-all-local-variables)
  (setq major-mode 'template-mode)
  (setq mode-name "Template")
  (template-mode-turn-on-font-lock)
  (run-hooks 'template-mode-hook))

(provide 'template-minor-mode)
(provide 'template-mode)
(provide 'outline-template-mode)

;;; template-mode.el ends here
