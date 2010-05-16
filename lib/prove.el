;;; prove.el --- Compilation mode for prove

;; Copyright (C) 2008 Philip Jackson

;; Author: Philip Jackson <phil@shellarchive.co.uk>
;; Version: 0.2

;; This file is not currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; (require 'prove)
;; M-x prove

;; Errors are foldable, you can toggle their visibility by putting
;; point on an error line and hitting tab. If you want folding
;; everytime set `prove-collapse-tree-on-finish' to non-nil.

;; If you use prove rather than perl to run the tests then make sure
;; you include the -v switch.

(require 'compile)

(defvar prove-mode-command "prove -v "
  "The command to offer by default when running `prove'.")
(defvar prove-mode-history nil
  "The previously run proves.")
(defvar prove-collapse-tree-on-finish nil
  "Collapse the tests when compilation has finished")

(add-to-list 'compilation-finish-functions 'prove-compile-finish)

;; ok, not ok faces
(copy-face 'font-lock-warning-face 'prove-mode-not-ok-face)
(copy-face 'font-lock-keyword-face 'prove-mode-ok-face)
(set-face-attribute 'prove-mode-ok-face nil
                    :foreground "LightGreen")
(set-face-attribute 'prove-mode-not-ok-face nil
                    :foreground "IndianRed3")

;; 2nd or 3rd last line (describing the overall failure)
(copy-face 'prove-mode-ok-face 'prove-mode-overall-success-face)
(copy-face 'prove-mode-not-ok-face 'prove-mode-overall-failure-face)
(set-face-attribute 'prove-mode-overall-success-face nil
                    :foreground "Green")
(set-face-attribute 'prove-mode-overall-failure-face nil
                    :foreground "Red")

;; test count
(copy-face 'prove-mode-not-ok-face 'prove-mode-test-count-face)
(set-face-attribute 'prove-mode-test-count-face nil
                    :foreground "Yellow")

(defvar prove-mode-font-lock-keywords
  '(("^\\(not ok\\) \\([[:digit:]]+\\)" . 'prove-mode-not-ok-face)
    ("^#.+$" . 'font-lock-comment-face)
    ("^ok \\([[:digit:]]+\\)" . 'prove-mode-ok-face)
    ("^All tests successful." . 'prove-mode-overall-success-face)
    ("^Failed [[:digit:]]+/[[:digit:]]+ test scripts"
     . 'prove-mode-overall-failure-face)
    ("^[[:digit:]]+\\.\\{2\\}[[:digit:]]+$"
     . 'prove-mode-test-count-face)
    ("^ok$" . 'prove-mode-overall-success-face)
    ("^dubious$" . 'prove-mode-overall-failure-face))
  "Faces for prove-mode.")

(defvar prove-mode-error-regexps
  (list
   (list (concat "#[[:blank:]]+\\(?:at\\|in\\) \\(.+?\\) "
                 "\\(?:at\\)? line \\([[:digit:]]+\\)") 1 2)
   (list ".+at \\(.+?\\) line \\([[:digit:]]+\\)" 1 2))
  "Hyperlink and highlight anything matching these.")

(define-compilation-mode prove-mode "Prove"
  "Prove compilation mode."
  (set (make-local-variable 'outline-regexp)
       (concat "^\\(\\(?:not \\)?ok [[:digit:]]+\\|"
               "[[:blank:]]Test returned status\\|"
               "All tests successful."
               "\\).*"))
  (set (make-local-variable 'outline-level)
       '(lambda () 1))
  (set (make-local-variable 'compilation-error-regexp-alist)
       prove-mode-error-regexps)
  (set (make-local-variable 'compilation-scroll-output) t)
  (set (make-local-variable 'compilation-mode-font-lock-keywords)
       prove-mode-font-lock-keywords)
  (local-set-key (kbd "<tab>") 'prove-toggle-headline)
  (outline-minor-mode))

(defun prove-toggle-headline ()
  "Toggle the visibility of a test."
  (interactive)
  (unless (looking-at outline-regexp)
    (outline-previous-heading))
  (if (get-char-property (point-at-eol) 'invisible)
      (show-subtree)
      (hide-subtree)))

(defun prove-compile-finish (buf status)
  "Hide all headline bodies."
  (when (and (string= mode-name "Prove")
             prove-collapse-tree-on-finish)
    (hide-body)
    (outline-previous-heading)
    (show-subtree)))

(defun prove-build-command ()
  "Construct a command to offer the user when `prove' is run. See
  `prove-mode-command'."
  prove-mode-command)

(defun prove (command-args)
  "Run prove (or perl) and highlight and linkify the resulting
tap."
  (interactive
   (list (read-from-minibuffer "Run perl/prove (like this): "
                               (prove-build-command)
                               nil
                               nil
                               'prove-mode-history)))
  (compilation-start command-args 'prove-mode))

(provide 'prove)
