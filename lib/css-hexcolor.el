;;; CSS-MODE COLOR VALUE COLORIZATION
;;;; CSS color values colored by themselves
;;;; after <http://xahlee.org/emacs/emacs_html.html>
(defvar hexcolor-keywords
  '(("#[abcdef[:digit:]]\\{3\\}"
     (0 (put-text-property
         (match-beginning 0)
         (match-end 0)
         'face (list :background
                     (hexcolor/tri-to-hex (match-string-no-properties 0))))))
    ("#[abcdef[:digit:]]\\{6\\}"
     (0 (put-text-property
         (match-beginning 0)
         (match-end 0)
         'face (list :background
                     (match-string-no-properties 0)))))))

(defun hexcolor/tri-to-hex (tri)
  "convert #abc to #aabbcc"
  (let (hex)
    (dolist (char (string-to-list tri))
      (if (char-equal char ?#)
          (setq hex (cons char hex))
        (setq hex (cons char (cons char hex)))))
    (concat (reverse hex))))

(defun hexcolor/add-to-font-lock ()
  (font-lock-add-keywords nil hexcolor-keywords))

(add-hook 'css-mode-hook 'hexcolor/add-to-font-lock)

(provide 'css-hexcolor)
