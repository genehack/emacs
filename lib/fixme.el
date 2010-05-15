;;; FIXME HIGHLIGHTING
;;;; modified from <http://ch.twi.tudelft.nl/~mostert/fixme.el>
(defvar fixme-modes '(emacs-lisp-mode
                      cperl-mode
                      )
  "modes to activate fixme comments in")

(defvar fixme-regexp  "FIXME" )

;; angry red on yellow background. It *really* gets your attention
(defface font-lock-fixme-face
  '((((class grayscale) (background light)) (:foreground "DimGray" :italic t))
    (((class grayscale) (background dark))  (:foreground "LightGray" :italic t))
    (((class color)     (background light)) (:foreground "Red"
                                             :background "Yellow"))
    (((class color)     (background dark))  (:foreground "Red"
                                             :background "Yellow")))
  "face for fixme comments. It should *really* get your attention")

(defvar font-lock-fixme-face 'font-lock-fixme-face)

(mapc
 (lambda (mode)
   (font-lock-add-keywords
    mode
    (list (list fixme-regexp 0 font-lock-fixme-face t))))
 fixme-modes)

(provide 'fixme)
