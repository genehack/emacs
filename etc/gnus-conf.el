;; gnus.el - gnus, bbdb, and related customizations

;;; INIT
(genehack/add-emacs-lib-subdir-to-load-path "bbdb/lisp")
(genehack/add-emacs-lib-subdir-to-load-path "gnus/lisp")

(add-to-list 'Info-default-directory-list (concat genehack/emacs-libs-dir "bbdb/texinfo"))
(add-to-list 'Info-default-directory-list (concat genehack/emacs-libs-dir "gnus/texi"))

(require 'adv-random-sig)
(require 'bbdb-autoloads)
(require 'bbdb)
(require 'bbdb-gnus)
(require 'gnus)
(require 'gnus-setup)
(require 'message)
(require 'nnmairix)

(setq gnus-init-file (concat genehack/emacs-config-dir "jsja-gnus"))
;;;; this fixes a problem with imap and authinfo files
(load-library "netrc")

;;; SELECT METHODS
(setq gnus-select-method '(nnnil))

;;; INSINUATIONS
(bbdb-initialize 'gnus 'message)
(bbdb-insinuate-gnus)
(bbdb-insinuate-message)

;;; HOOK MODS (other hooks set later too)
;;  bbdb
(add-hook 'bbdb-change-hook 'bbdb-timestamp-hook)
(add-hook 'bbdb-create-hook 'bbdb-creation-date-hook)
;;  gnus
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
(add-hook 'gnus-save-quick-newsrc-hook 'turn-off-newsrc-backup)
(add-hook 'gnus-save-standard-newsrc-hook 'turn-off-newsrc-backup)
;;  message
(add-hook 'message-mode-hook
          (lambda ()
            (footnote-mode)
            (auto-fill-mode 1)
            (filladapt-mode 1)
            (font-lock-mode 1)
            (flyspell-mode 1)
            (setq fill-column 72)))
(add-hook 'message-send-hook 'ispell-message)
(add-hook 'message-setup-hook 'bbdb-define-all-aliases)

;;; BINDINGS
(define-key message-mode-map (kbd "M-n") 'genehack/change-message-subject)
(define-key gnus-summary-mode-map (kbd "C-c s")
  '(lambda () (interactive)
     (gnus-summary-move-article nil "nnimap:SPAM-MISSES")))

;;; ADV-SIGNATURE
(setq adv-signature-dir (expand-file-name "~/doc/quote")
      adv-signature-greeting (expand-file-name "~/doc/sig-pre"))

;;; ARCHIVE OPTIONS
(setq gnus-gcc-mark-as-read t)

;;; ARTICLE OPTIONS
(setq
 gnus-visible-headers '( "^Date:"
                         "^Newsgroups:"
                         "^To:"
                         "^CC:"
                         "^From:"
                         "^Reply-To:"
                         "^Organization:"
                         "^Subject:"
                         "^X-Spam-Status:"
                         "^Approved:")
 gnus-sorted-header-list gnus-visible-headers
 gnus-boring-article-headers '(empty reply-to to-address)
 gnus-signature-separator '( "^-- $" )
 mm-attachment-override-types '("image/.*")
 )

;;; ARTICLE TREATMENTS
(setq gnus-treat-body-boundary 'head
      gnus-treat-buttonize t
      gnus-treat-buttonize-head 'head
      gnus-treat-date-local 'head
      gnus-treat-date-ut 'head
      gnus-treat-display-face nil
      gnus-treat-display-smileys nil
      gnus-treat-display-x-face nil
      gnus-treat-emphasize t
      gnus-treat-fill-article nil
      gnus-treat-fill-long-lines nil
      gnus-treat-from-picon nil
      gnus-treat-hide-boring-headers 'head
      gnus-treat-highlight-citation t
      gnus-treat-highlight-headers 'head
      gnus-treat-highlight-signature t
      gnus-treat-mail-picon nil
      gnus-treat-newsgroups-picon nil
      gnus-treat-unsplit-urls nil
      gnus-treat-wash-html nil
      gnus-treat-x-pgp-sig 'head)


;;; GROUP OPTIONS
(setq gnus-auto-select-first nil
      gnus-auto-select-next 'quietly
      gnus-large-newsgroup 100000)

;;; TOPIC OPTIONS
(setq gnus-topic-display-empty-topics t)

;;; HIGHLIGHTING OPTIONS
(setq gnus-group-highlight '(((and mailp (< level 4) (> unread 0)) . genehack/gnus-mail-unread-face)
                             ((and mailp (< level 4) (> ticked 0)) . genehack/gnus-mail-ticked-face)
                             ((and mailp (< level 4)             ) . genehack/gnus-mail-group-face)
                             (     mailp                           . genehack/gnus-dead-mail-group-face)
                             ((and (< level 7) (> unread 0)      ) . genehack/gnus-unread-face)
                             ((and (< level 7) (> ticked 0)      ) . genehack/gnus-ticked-face)
                             ((< level 7                         ) . genehack/gnus-group-face)
                             (t                                    . genehack/gnus-dead-group-face))
      gnus-summary-selected-face 'genehack/gnus-summary-selected-face
      gnus-summary-highlight '(((eq mark gnus-canceled-mark) . genehack/gnus-summary-canceled-face)
                               ((eq mark gnus-ticked-mark  ) . genehack/gnus-summary-ticked-face)
                               ((eq mark gnus-dormant-mark ) . genehack/gnus-summary-dormant-face)
                               ((eq mark gnus-unread-mark  ) . genehack/gnus-summary-unread-face)
                               (t                            . genehack/gnus-summary-read-face)))

;;; SCORING OPTIONS
(setq gnus-use-scoring nil
      gnus-use-adaptive-scoring nil)

;;; SUMMARY OPTIONS
(setq gnus-summary-display-arrow t)

;;; BBDB/GNUS OPTIONS
(setq gnus-use-bbdb t
      bbdb/gnus-summary-prefer-bbdb-data t
      bbdb/gnus-summary-prefer-real-names t)

;;; THREADING AND SORTING OPTIONS
(setq gnus-article-sort-functions '(gnus-article-sort-by-number
                                    gnus-article-sort-by-date  )
      gnus-build-sparse-threads 'some
      gnus-show-threads t
      gnus-thread-sort-functions '(gnus-thread-sort-by-number
                                   gnus-thread-sort-by-subject
                                   gnus-thread-sort-by-date    )
      gnus-thread-indent-level 2
      gnus-summary-gather-subject-limit 'fuzzy
      gnus-fetch-old-headers 'some)

;;; LINE FORMATS
(setq gnus-article-mode-line-format "%S"
      gnus-group-line-format ":%p%B%S%m%(%-30g%)%45= .. [ %4y unread | %4T ticked | %3I dormant ]\n"
      gnus-summary-line-format ":%U%R%ub<%d> [ %6i ] { %5L lines } %(%-40f%) | %I%s\n"
      gnus-summary-mode-line-format "[ %p ] ( %Z ) art: %A sub: %S"
      gnus-topic-line-format ":%i%(%-30n%)%45= -- < %4g groups | %4A unread >%v\n")

;;; MESSAGE OPTIONS
(setq mail-user-agent 'gnus-user-agent
      message-from-style 'angles
      message-make-forward-subject-function '(message-forward-subject-fwd)
      message-subject-re-regexp "^\\(\\(\\([Rr][Ee]\\|[Ss][Vv]\\|[Aa][Ww]\\): *\\)+\\)"
      message-subject-trailing-was-query 'ask
      message-wash-forwarded-subjects t)

;;; MISC OPTIONS
(setq gnus-ignored-from-addresses genehack/mail-addresses
      gnus-novice-user nil
      gnus-summary-same-subject "->"
      gnus-user-agent '(emacs-gnus-type))

;;; BBDB: MISC
(setq bbdb-always-add-addresses 'ask
      bbdb-default-area-code 301
      bbdb-new-nets-always-primary 'never
      bbdb-notice-auto-save-file t
      bbdb-north-american-phone-numbers-p t
      bbdb-offer-save 'auto
      bbdb-pop-up-target-lines 5
      bbdb-quiet-about-name-mismatches 2
      bbdb-send-mail-style 'message
      bbdb-user-mail-names genehack/mail-addresses
      bbdb/mail-auto-create-p nil
      bbdb/news-auto-create-p nil)

;;; BBDB/SEND
(autoload 'bbdb/send-hook "moy-bbdb"
  "Auto-notice on send")
(add-hook 'message-send-hook 'bbdb/send-hook)
(setq bbdb/send-auto-create-p t)
(setq bbdb/send-prompt-for-create-p t)

;;; FIX M$ LOSSAGE
(standard-display-ascii ?\205 "...")
(standard-display-ascii ?\221 [?\'])
(standard-display-ascii ?\222 [?\'])
(standard-display-ascii ?\223 [?\"])
(standard-display-ascii ?\224 [?\"])
(standard-display-ascii ?\225 [?\*])
(standard-display-ascii ?\226 "---")
(standard-display-ascii ?\227 "--")


;;; FUNCTIONS
(defun genehack/change-message-subject (new-subject)
  "change the subject"
  (interactive "sNew subject: ")
  (let ((case-fold-search nil))
    (goto-line 0)
    (re-search-forward "^Subject: ")
    (if (re-search-forward "R[Ee]: " nil t)
        (replace-match "")
      )
    (insert-string new-subject)
    (insert-string " (was: ")
    (end-of-line)
    (insert-string ")")))

(defvar genehack/last-mail-check (float-time)
  "last time i checked mail via magic key")
(defvar genehack/mail-check-interval (* 60 5)
  "how long i should wait before checking mail")
(defun genehack/gnus ()
  "Bring Gnus *Group* buffer to front, starting Gnus if needed"
  (interactive)
  (if (or (not (fboundp 'gnus-alive-p))
          (not (gnus-alive-p)))
      (gnus)
    (switch-to-buffer "*Group*")
    (delete-other-windows)
    (setq next-mail-check (+ genehack/last-mail-check genehack/mail-check-interval))
    (if (< (float-time) next-mail-check)
        (message (concat
                  "no gnus is good news ("
                  (int-to-string (floor (- next-mail-check (float-time))))
                  " seconds until next check allowed)" ))
      (gnus-group-get-new-news 1)
      (setq genehack/last-mail-check (float-time)))))

;;  from english.kakeboksen.org/archives/000245.html
(defun run-command-on-article (command &optional replace)
  "Run the full text of the current article through a filter command.

The full text of the current article is run through the specificied
COMMAND as a filter. The output of the command is returned."
  (interactive "sCommand: \nP")
  (let ((n (gnus-summary-article-number))
        (g gnus-newsgroup-name))
    (with-temp-buffer
      (gnus-request-article n g (current-buffer))
      (shell-command-on-region
       (point-min) (point-max)
       command)
      (if replace
          (gnus-request-replace article n g (current-buffer)))
      )))

(defun turn-off-newsrc-backup ()
  "turn off backup .newsrc creation"
  (set (make-local-variable 'backup-inhibited) t))

;;; FACES
(make-face 'genehack/gnus-mail-unread-face)
(set-face-foreground 'genehack/gnus-mail-unread-face "Blue")

(make-face 'genehack/gnus-mail-ticked-face)
(set-face-foreground 'genehack/gnus-mail-ticked-face "Red")

(make-face 'genehack/gnus-mail-group-face)
(set-face-foreground 'genehack/gnus-mail-group-face "Grey")

(make-face 'genehack/gnus-dead-mail-group-face)
(set-face-foreground 'genehack/gnus-dead-mail-group-face "Yellow")

(make-face 'genehack/gnus-unread-face)
(set-face-foreground 'genehack/gnus-unread-face "Blue")

(make-face 'genehack/gnus-ticked-face)
(set-face-foreground 'genehack/gnus-ticked-face "Red")

(make-face 'genehack/gnus-group-face)
(set-face-foreground 'genehack/gnus-group-face "Grey")

(make-face 'genehack/gnus-dead-group-face)
(set-face-foreground 'genehack/gnus-dead-group-face "Yellow")

(make-face 'genehack/gnus-summary-selected-face)
(set-face-foreground 'genehack/gnus-summary-selected-face "Yellow")

(make-face 'genehack/gnus-summary-cancelled-face)
(set-face-foreground 'genehack/gnus-summary-cancelled-face "Black")
(set-face-background 'genehack/gnus-summary-cancelled-face "Red")

(make-face 'genehack/gnus-summary-ticked-face)
(set-face-foreground 'genehack/gnus-summary-ticked-face "Red")

(make-face 'genehack/gnus-summary-dormant-face)
(set-face-foreground 'genehack/gnus-summary-dormant-face "Green")

(make-face 'genehack/gnus-summary-unread-face)
(set-face-foreground 'genehack/gnus-summary-unread-face "Blue")

(make-face 'genehack/gnus-summary-read-face)
(set-face-foreground 'genehack/gnus-summary-read-face "Grey")

(make-face 'gnus-emphasis-bold)
(set-face-bold-p 'gnus-emphasis-bold t)

(make-face 'gnus-emphasis-italic)
(set-face-italic-p 'gnus-emphasis-italic t)

(make-face 'gnus-emphasis-underline)
(set-face-underline-p 'gnus-emphasis-underline t)

(make-face 'gnus-emphasis-bold-italic)
(set-face-bold-p 'gnus-emphasis-bold-italic t)
(set-face-italic-p 'gnus-emphasis-bold-italic t)

(make-face 'gnus-emphasis-underline-italic)
(set-face-underline-p 'gnus-emphasis-underline-italic t)
(set-face-italic-p 'gnus-emphasis-underline-italic t)

(make-face 'gnus-emphasis-underline-bold)
(set-face-underline-p 'gnus-emphasis-underline-bold t)
(set-face-bold-p 'gnus-emphasis-underline-bold t)

(make-face 'gnus-emphasis-underline-bold-italic)
(set-face-underline-p 'gnus-emphasis-underline-bold-italic t)
(set-face-bold-p 'gnus-emphasis-underline-bold-italic t)
(set-face-italic-p 'gnus-emphasis-underline-bold-italic t)

(make-face 'gnus-cite-attribution-face)
(set-face-foreground 'gnus-cite-attribution-face      "Gray60")

(make-face 'gnus-cite-face-1)
(set-face-foreground 'gnus-cite-face-1                "Blue")

(make-face 'gnus-cite-face-2)
(set-face-foreground 'gnus-cite-face-2                "Red")

(make-face 'gnus-cite-face-3)
(set-face-foreground 'gnus-cite-face-3                "Green")

(make-face 'gnus-header-content-face)
(set-face-foreground 'gnus-header-content-face        "White")

(make-face 'gnus-header-from-face)
(set-face-foreground 'gnus-header-from-face           "Blue")

(make-face 'gnus-header-name-face)
(set-face-foreground 'gnus-header-name-face           "Gray60")

(make-face 'gnus-header-subject-face)
(set-face-foreground 'gnus-header-subject-face        "SlateBlue")

(make-face 'gnus-signature-face)
(set-face-foreground 'gnus-signature-face             "Gray")
