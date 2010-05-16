;; erc.el - ERC customizations
(require 'erc)
(require 'erc-nicklist)

;;; MODULES
(setq erc-modules '(autoaway
                    irccontrols
                    menu
                    notify
                    ring
                    scrolltobottom
                    services
                    smiley
                    spelling
                    track
                    truncate))
(erc-update-modules)

;;; BASICS
(setq erc-port 6667
      erc-prompt-for-password nil
      erc-server "irc.freenode.net"
      perl-erc-server "irc.perl.org"
      oftc-erc-server "irc.oftc.net"
      erc-email-userid "genehack@genehack.org"
      erc-nick "genehack"
      erc-user-full-name "John SJ Anderson")

(defun genehack/erc ()
  "start IRC or switch to channel buffer with activity"
  (interactive)
  (if (bound-and-true-p genehack/erc-running)
      (erc-track-switch-buffer 1)
    (erc :server erc-server :port erc-port :nick erc-nick :full-name erc-user-full-name)
    (erc :server perl-erc-server :port erc-port :nick erc-nick :full-name erc-user-full-name)
    (erc :server oftc-erc-server :port erc-port :nick erc-nick :full-name erc-user-full-name)
    (setq genehack/erc-running t)))

(setq erc-auto-discard-away t
      erc-auto-set-away t
      erc-autoaway-idle-seconds 600)
(erc-autoaway-reestablish-idletimer)

(erc-autojoin-mode 1)
(setq erc-autojoin-channels-alist
      '((".*\\.freenode.net" "#dc-sage" "#emacs" "#openmelody")
        (".*\\.oftc.net" "#linode")
        ("irc\\.perl\\.org\\|.+\\.shadowcat\\.co\\.uk\\|.*\\.llarian\\..net" "#catalyst" "#corehackers" "#dbix-class" "#moose" "#perl" "#ppw" "#yapc")))

(erc-fill-mode t)
(setq erc-fill-prefix "         ")

(setq erc-current-nick-highlight-type 'nick-or-keyword)

(erc-match-mode 1)
(setq erc-keywords '("genehack"))

(defvar genehack/erc-mode-format "[%a] %n on %t <%m> LAG:%l"
  "My mode format string for erc buffers")
(setq erc-mode-line-format genehack/erc-mode-format
      erc-header-line-format genehack/erc-mode-format)

(erc-netsplit-mode t)

(erc-services-mode 1)
(setq erc-prompt-for-nickserv-password nil)

(setq erc-auto-query 'buffer)

(erc-ring-mode t)

(erc-spelling-mode 1)

(erc-timestamp-mode t)
(setq erc-insert-timestamp-function 'erc-insert-timestamp-left
      erc-timestamp-format "%T "
      erc-timestamp-only-if-changed-flag nil)

(erc-track-mode t)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477"))
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))
