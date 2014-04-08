;;; mu4e --- mu4e stuff

;;; Commentary:

;;; Code:

(add-to-list 'load-path "/opt/mu/share/emacs/site-lisp/mu4e")
(require 'mu4e)
(setq mu4e-debug t)
(setq mu4e-mu-binary "/opt/mu/bin/mu")
(setq mu4e-maildir "~/Mail") ;; top-level Maildir
(setq mu4e-sent-folder "/Sent") ;; where do i keep sent mail?
(setq mu4e-drafts-folder "/Drafts") ;; where do i keep half-written mail?
(setq mu4e-trash-folder "/Deleted") ;; where do i move deleted mail?
(setq mu4e-get-mail-command "/home/genehack/src/offlineimap/offlineimap.py")
;(setq mu4e-update-interval 900) ;; update every X seconds
(setq mu4e-html2text-command "/opt/w3m/bin/w3m -dump -T text/html")
(setq mu4e-view-prefer-html t)
(setq mu4e-use-fancy-chars t)
(set-language-environment "UTF-8")

;(setq mu4e-maildir-shortcuts
;      '(("/YouGov/INBOX"     . ?i)
;        ("/GMail/INBOX"   . ?g)
;        ("/YouGov/archive" . ?a)
;        ("/YouGov/error emails" . ?e)))

;; (setq mu4e-bookmarks
;;       '( ("flag:unread AND NOT flag:trashed" "Unread messages"      ?u)
;;          ("date:today..now"                  "Today's messages"     ?t)
;;          ("date:7d..now"                     "Last 7 days"          ?w)
;;          ("mime:image/*"                     "Messages with images" ?i)
;;          ("\"maildir:/YouGov/error emails\" subject:paix" "PAIX Errors" ?p)
;;          ("\"maildir:/YouGov/error emails\" subject:ldc" "LDC Errors" ?l)))


;; (setq w3m-command "/usr/local/bin/w3m")

;; sending mail
;; (setq message-send-mail-function 'message-send-mail-with-sendmail
;;       sendmail-program "/usr/local/bin/msmtp"
;;       user-full-name "Eric Larson")

;; ;; Choose account label to feed msmtp -a option based on From header
;; ;; in Message buffer; This function must be added to
;; ;; message-send-mail-hook for on-the-fly change of From address before
;; ;; sending message since message-send-mail-hook is processed right
;; ;; before sending message.
;; (defun choose-msmtp-account ()
;;   (if (message-mail-p)
;;       (save-excursion
;;         (let*
;;             ((from (save-restriction
;;                      (message-narrow-to-headers)
;;                      (message-fetch-field "from")))
;;              (account
;;               (cond
;;                ((string-match "eric.larson@yougov.com" from) "yougov")
;;                ((string-match "eric@ionrock.org" from) "gmail")
;;                ((string-match "ionrock@gmail.com" from) "gmail"))))
;;           (setq message-sendmail-extra-arguments (list '"-a" account))))))
;; (setq message-sendmail-envelope-from 'header)
;; (add-hook 'message-send-mail-hook 'choose-msmtp-account)

(provide 'mu4e)
;;; mu4e.el ends here
