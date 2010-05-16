;; Shut up.

(defun maybe-fbind (args)
  (while args
    (or (fboundp (car args))
	(defalias (car args) 'ignore))
    (setq args (cdr args))))

(defun maybe-bind (args)
  (mapcar (lambda (var) (unless (boundp var) (set var nil))) args))

(unless (featurep 'xemacs)
  (maybe-fbind '(pgg-display-output-buffer url-generic-parse-url))
  (maybe-bind '(help-xref-stack-item
		url-version w3-meta-charset-content-type-regexp
		w3-meta-content-type-charset-regexp))

  (when (<= emacs-major-version 22)
    (defun nnkiboze-score-file (a))
    (maybe-fbind
     '(Info-index
       Info-index-next Info-menu bbdb-complete-name display-time-event-handler
       epg-check-configuration find-coding-system frame-device w3-do-setup
       w3-prepare-buffer w3-region w32-focus-frame w3m-detect-meta-charset
       w3m-region)))

  (when (= emacs-major-version 21)
    (defun split-line (&optional arg))
    (maybe-fbind
     '(clear-string
       custom-autoload delete-annotation delete-extent device-connection
       dfw-device events-to-keys find-face font-lock-set-defaults
       get-char-table glyph-height glyph-width help-buffer ldap-search-entries
       mail-aliases-setup make-annotation make-event make-glyph
       make-network-process map-extents message-xmas-redefine put-char-table
       run-mode-hooks set-extent-property set-itimer-function
       set-keymap-default-binding temp-directory unicode-precedence-list
       url-generic-parse-url url-http-file-exists-p
       valid-image-instantiator-format-p vcard-pretty-print
       w3-coding-system-for-mime-charset window-pixel-height
       window-pixel-width))
    (maybe-bind
     '(eudc-protocol
       filladapt-mode help-echo-owns-message itimer-list ps-print-color-p
       w3-meta-charset-content-type-regexp
       w3-meta-content-type-charset-regexp))))

(when (featurep 'xemacs)
  (defun nnkiboze-score-file (a))
  (defun split-line (&optional arg))
  (maybe-fbind
   '(clear-string
     codepage-setup create-image detect-coding-string
     display-time-event-handler epg-check-configuration event-click-count
     event-end event-start find-coding-systems-for-charsets
     find-coding-systems-region find-coding-systems-string find-image
     help-buffer image-size image-type-available-p insert-image
     mail-abbrevs-setup make-mode-line-mouse-map make-network-process
     mouse-minibuffer-check mouse-movement-p mouse-scroll-subr
     pgg-display-output-buffer posn-point posn-window put-image read-event
     select-safe-coding-system sort-coding-systems track-mouse
     url-generic-parse-url url-http-file-exists-p vcard-pretty-print
     w3m-detect-meta-charset window-edges))
  (maybe-bind
   '(adaptive-fill-first-line-regexp
     buffer-display-table cursor-in-non-selected-windows
     default-enable-multibyte-characters eudc-protocol filladapt-mode
     gnus-agent-expire-current-dirs help-xref-stack-item line-spacing
     mark-active mouse-selection-click-count mouse-selection-click-count-buffer
     rmail-insert-mime-forwarded-message-function show-trailing-whitespace
     tool-bar-mode transient-mark-mode url-version
     w3-meta-charset-content-type-regexp w3-meta-content-type-charset-regexp))

  (when (or (and (= emacs-major-version 21) (= emacs-minor-version 4))
	    (featurep 'sxemacs))
    (maybe-fbind
     '(custom-autoload
       display-graphic-p display-images-p display-visual-class
       select-frame-set-input-focus unicode-precedence-list w32-focus-frame
       x-focus-frame))
    (maybe-bind
     '(default-file-name-coding-system)))

  (unless (featurep 'mule)
    (maybe-fbind
     '(ccl-execute-on-string
       charsetp coding-system-get get-charset-property
       pgg-display-output-buffer pgg-parse-crc24-string
       unicode-precedence-list))
    (maybe-bind
     '(current-language-environment
       default-file-name-coding-system language-info-alist pgg-parse-crc24)))

  (unless (featurep 'file-coding)
    (maybe-fbind
     '(coding-system-base
       coding-system-change-eol-conversion coding-system-list coding-system-p
       find-coding-system))
    (maybe-bind
     '(buffer-file-coding-system
       coding-system-for-read coding-system-for-write
       enable-multibyte-characters file-name-coding-system))))

(provide 'lpath)

;;; arch-tag: d1ad864f-dca6-4d21-aa3f-be3248e66dba
