(provide 'bbdb-autoloads)
(eval-when-compile
  (condition-case ()
      (require 'custom)
    (error nil))
  (if (and (featurep 'custom) (fboundp 'custom-declare-variable))()
    (defmacro defgroup (&rest args)
      nil)
    (defmacro defcustom (var value doc &rest args)
      `(defvar ,var ,value ,doc))
    (defmacro defface (var value doc &rest args)
      `(make-face ,var))
    (defmacro define-widget (&rest args)
      nil)))

