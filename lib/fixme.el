;;; FIXME HIGHLIGHTING
;;;; modified from <http://ch.twi.tudelft.nl/~mostert/fixme.el>
(defvar fixme-modes '(emacs-lisp-mode
                      cperl-mode
                      )
  "modes to activate fixme comments in")

(defvar fixme-regexp "FIXME")
(defvar todo-regexp  "TODO")
(defvar bug-regexp   "BUG")

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

(defface font-lock-todo-face
  '((((class grayscale) (background light)) (:foreground "DimGray" :italic t))
    (((class grayscale) (background dark))  (:foreground "LightGray" :italic t))
    (((class color)     (background light)) (:foreground "Green"
                                             :background "White"))
    (((class color)     (background dark))  (:foreground "Green"
                                             :background "White")))
  "face for todo comments.")
(defvar font-lock-todo-face 'font-lock-todo-face)

(defface font-lock-bug-face
  '((((class grayscale) (background light)) (:foreground "DimGray" :italic t))
    (((class grayscale) (background dark))  (:foreground "LightGray" :italic t))
    (((class color)     (background light)) (:foreground "White"
                                             :background "Red"))
    (((class color)     (background dark))  (:foreground "White"
                                             :background "Red")))
  "face for bug comments. It should *really* get your attention")
(defvar font-lock-bug-face 'font-lock-bug-face)

(mapc
 (lambda (mode)
   (font-lock-add-keywords
    mode
    (list
     (list fixme-regexp 0 font-lock-fixme-face t)
     (list todo-regexp 0 font-lock-todo-face t)
     (list bug-regexp 0 font-lock-bug-face t))))
 fixme-modes)

(provide 'fixme)
