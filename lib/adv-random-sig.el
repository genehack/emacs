;;; adv-random-sig.el --- advanced random signature inserter

;; Copyright (c) 2000 Raymond Scholz

;; $Id: adv-random-sig.el 930 2007-10-08 14:46:32Z genehack $

;; Author 	   : Raymond Scholz <ray@zonix.de>
;; Maintainer 	   : Raymond Scholz <ray@zonix.de>
;; Bug Reports     : adv-random-sig-bugs@zonix.de
;; Web Site        : http://www.zonix.de/div/adv-random-sig.el
;; Keywords: news, mail, signature, random

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is an advanced random signature inserter for message-mode
;; (probably other modes for composing mail and news) and for use with
;; Gnus' posting styles.
;;
;; It is heavily based on random-sig.el by Matt Simmons
;; <simmonmt@acm.org>, http://www.netcom.com/~simmonmt.
;;
;; Warning: This is the first lisp thing I've ever done since I left
;; the warm and comfort home of my own .emacs and .gnus.  It may,
;; well, it _will_ contain awkward lisp statements and errors.  But I
;; can assure you that it works for my environment and didn't cause
;; any harm so far.
;;
;; BUILDING A SIGNATURE
;; --------------------
;;
;; A signature is built as follows:
;;
;; -- 
;; [head]
;; random quote
;;
;; The head is a string, constant to all signatures (F.e. an URL or
;; your email address).  Point `adv-signature-head' to a file with the
;; contents of that head.  The use of a signature head is optional,
;; set `adv-signature-head' to `nil' if you don't want it.
;;
;; The quote (or whatever) is taken from a randomly chosen file in the
;; directory specified by `adv-signature-dir'.  Every quote is stored
;; in a single file, the file name doesn't matter.  To exstinguish all
;; randomness, you can set `adv-signature-dir' to a file.
;;
;; Another feature is the insertion of a greeting formula _before_ the
;; signature seperator.  If you're as lazy as myself, you'll find this
;; useful.  Put the greeting in a file and point
;; `adv-signature-greeting' to it.  As `adv-signature-head' this is
;; optional - set `adv-signature-greeting' to `nil' if you don't need
;; it.  Note that this doesn't interact well with Gnus' message-mode.
;; You'll have to manually remove the signature seperator that Gnus
;; places _before_ the greeting.
;;
;; Note that you can savely place the greeting formula and the head
;; within `adv-signature-dir' as they are excludes from the random
;; choice.
;;
;; USAGE
;; -----
;;
;; Put this file in a directory covered by your load-path and place
;; the following in your .emacs or .gnus:
;;
;; (require 'adv-random-sig)
;;
;; Make the variables point to the apropriate directory resp. files:
;;
;; (setq adv-signature-dir (expand-file-name "~/.sigs/quotes/"))
;;
;; (setq adv-signature-head nil) ; don't use the head
;; ;(setq adv-signature-head (expand-file-name "~/.sigs/head/myurl"))
;; 
;; (setq adv-signature-greeting nil) ; don't use the greeting formula
;; ;(setq adv-signature-greeting (expand-file-name "~/.sigs/greet/english"))
;;
;;
;; There are several functions you can call to mess up your mail or
;; news posting with signatures, heads and greeting formulas.  Now
;; it's up to you, to choose the one, that fits your purposes.
;;
;; FLAVOURS
;; --------
;;
;; Important notice: in contrast to the originial random-sig.el you
;; can influence the decision on what random quote will be used in a
;; certain manner.  Every user interface function below can have an
;; optional argument, that constraints the quote to a certain
;; "flavour".  A flavour simply is a subdirectory to
;; `adv-signature-dir'.  Place quotes of a certain flavour in that
;; subdirectory and see the example:
;;
;; ~/.sig/quotes/
;; ~/.sig/quotes/linux/
;; ~/.sig/quotes/linux/lin1
;; ~/.sig/quotes/linux/lin2
;; ~/.sig/quotes/humor/
;; ~/.sig/quotes/humor/hum1
;; ~/.sig/quotes/humor/hum2
;; ~/.sig/quotes/quote1
;;
;; Setting `adv-signature-dir' to "~/.sig/quotes/" the flavour "linux"
;; will choose between lin1 and lin2.  If flavour is "linux/lin2", the
;; only choice is lin2.  If the flavour is "" or not specified, the
;; only choice is quote1.  Sorry, the choice is not recursive, anyone
;; who can do that?
;;
;; FUNCTIONS
;; ---------
;;
;; `adv-random-signature-no-sep' returns a random signature without
;; the usual seperator.  This is meant to be used with Gnus'
;; message-mode, which is hard to convince _not_ to insert the "-- "
;; line.  If you use posting styles, an example would be:
;;
;; (setq gnus-posting-styles
;;   '(
;;      ((message-news-p)
;;	 (signature adv-random-signature-no-sep))
;;
;;      ("^comp\\.os\\.linux.*"
;;       (signature adv-random-signature-no-sep "linux")) 
;;    [...]
;;
;; [$ray$ lacks of an example for calling from a hook!]
;;
;; `adv-random-signature' does the same thing as
;; `adv-random-signature-no-sep' but adds a signature seperator in
;; front and makes use of `adv-signature-head' if non nil.
;;
;; `adv-random-signature-fun' is ment to be a backward compatible
;; version of `random-signature-fun' in the original random-sig.el.
;; Note, that the names of the variables compared with random-sig.el
;; have change too!
;;
;; $Log: adv-random-sig.el,v $
;; Revision 1.2  2000/10/28 15:08:30  rscholz
;; Name function adv-random-signature-no-sep instead of adv-random-signature-w/o-sep.
;;
;; Revision 1.1  2000/10/28 14:47:31  rscholz
;; Initial revision
;;
;;

;; variables

(defvar adv-signature-dir (expand-file-name "~/.sigs/quotes/")
  "*The directory from which the random signature quotes are extracted.

Note that this variable can also point to a file to exstinguish all
randomness.")

(defvar adv-signature-head (expand-file-name "~/.sigs/head")
  "*The file used as a sig header.  F.e. put an URL and your email
address in that file.

Set to nil if the header is not to be used.")

(defvar adv-signature-greeting nil
  "*The file used as a pre-signature greeting in
`adv-signature-greeting'.  F.e. put your name in that file.

Set to nil if the greeting is not to be used.")

;; internal stuff

(defun adv-signature-head-string ()
  "Return the content of the file given in `adv-signature-head'."

  (with-temp-buffer
    (if (file-readable-p adv-signature-head)
	(insert-file-contents adv-signature-head)
      (insert (concat "*** Unable to insert header file '"
			     adv-signature-head "' ***\n")))
    (buffer-string)))


(defun adv-signature-quote-string  (&optional flavour)
  "Return a signature quote from a random file placed in
`adv-signature-dir'.  If `adv-signature-dir' points to a file itself,
return the contents of that file.

Optional argument is a FLAVOUR that constraints the choice to a
subdirectory or file in `adv-signature-dir'."

  (with-temp-buffer
    (let ((sig-dir-file
	   (if (stringp flavour)
	       (concat adv-signature-dir flavour)
	     adv-signature-dir)))
      
      ;; if it is a directory, randomly choose a quote
      (if (file-directory-p sig-dir-file)
	  (let ((sig-files (delete adv-signature-greeting 
				   (delete adv-signature-head
					   (directory-files sig-dir-file
							    t "[^/][^\\.][^\\.]?$")))))

	    (insert-file-contents (nth (random (length sig-files)) sig-files)))

	;; if it is a file, use it
	(if (file-readable-p sig-dir-file)
	    (insert-file-contents sig-dir-file)

	  ;; none of that
	  (insert (concat "*** Unable to find quote file or directory '"
				 sig-dir-file "' ***\n")))))

    (buffer-string)))


(defun adv-signature-greeting-string ()
  "Return the contents of the file given in `adv-signature-greeting'."

  (with-temp-buffer
    (if (file-readable-p adv-signature-greeting)
	(insert-file-contents adv-signature-greeting)
      (insert (concat "*** Unable to insert greeting file '"
			     adv-signature-greeting "' ***\n")))
    (buffer-string)))


;; user interface

(defun adv-random-signature-no-sep (&optional flavour)
  "Build a signature with head and a random quote but without seperator.

Optional argument is a FLAVOUR that constraints the choice to a
subdirectory or file in function `adv-signature-quote-string'."
  
  (with-temp-buffer
    
    ;; Set up message buffer for signature insertion
    (goto-char (point-max))
    (unless (bolp)
      (insert "\n"))

    ;; Get the header (if necessary)
    (if adv-signature-head
	(insert (adv-signature-head-string)))
    
    ;; Get the random quote
    (goto-char (point-max))
    (unless (bolp)
      (insert "\n"))

    (insert (adv-signature-quote-string flavour))

    (buffer-string)))


(defun adv-random-signature (&optional flavour)
  "Build a signature with greeting (if non nil), seperator, head (if
non nil) and a random quote.

Optional argument is a FLAVOUR that constraints the choice to a
subdirectory or file in function `adv-signature-quote-string'."

  (with-temp-buffer
    
    ;; Set up message buffer for signature insertion
    (goto-char (point-max))
    (unless (bolp)
      (insert "\n"))

    ;; Get the greeting (if necessary)
    (if adv-signature-greeting
	(insert (adv-signature-greeting-string)))
    
    ;; Set up message buffer for signature insertion
    (goto-char (point-max))
    (unless (bolp)
      (insert "\n"))

    ;; Insert signature seperator
    (insert "-- \n")
    
    (insert (adv-random-signature-no-sep flavour))

    (buffer-string)))


;; backward compatibility with random-sig.el	
(defun adv-random-signature-fun (&optional flavour)
  "Insert a random signature in the current buffer.

Optional argument is a FLAVOUR that constraints the choice to a
subdirectory or file in function `adv-signature-quote-string'."

  (interactive)

  ;; Set up message buffer for signature insertion
  (goto-char (point-max))
  (unless (bolp)
    (insert "\n"))

  (insert (adv-random-signature flavour)))


(provide 'adv-random-sig)

;; end of adv-random-sig.el
