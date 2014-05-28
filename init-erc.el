;;; erc
;; Check channels
(erc-track-mode t)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477"))
(setq erc-autojoin-channels-alist
      '(("freenode.net"
         "#emacs"
         "#ruby-lang"
         "#ruby.fr"
         "#ruby"
         "#git-fr"
         "#archlinux"
         "#archlinux-fr"
         "#emacsfr"
         "#flexget"
         "#linux-fr"
         "#debianfr"
         "#TikZ"
         "#org-mode-fr")))

;; Flyspell input line with different dictionaries
(erc-spelling-mode 1)
(mapc (lambda (chan)
        (mapc (lambda (name)
                (if (string-match "fr$" name)
                    (push (list name "fr") erc-spelling-dictionaries)
                  (push (list name "en") erc-spelling-dictionaries)))
              (cdr chan)))
      erc-autojoin-channels-alist)

(erc-match-mode 1)
(setq erc-keywords '("magit" "koans" "rubywarrior" "org"))

;; Custom prompt
(setq erc-prompt
      (lambda () (concat (buffer-name) ">")))

(defvar erc-notifications-ring-size 5
  "Maximum number of simultaneous notifications.")

(defvar erc-notifications-ring (make-ring (1+ erc-notifications-ring-size))
  "Ring of notifications id.")

(with-eval-after-load 'erc-desktop-notifications
  (setq erc-notifications-icon "emacs-snapshot")
  (defun erc-notifications-notify-on-match (match-type nickuserhost msg)
    (let ((nick (nth 0 (erc-parse-user nickuserhost))))
      (unless (or (string-match-p "^Server:" nick)
                  (when (boundp 'erc-track-exclude)
                    (member nick erc-track-exclude)))
        (dbus-ignore-errors
          (ring-insert erc-notifications-ring
                (notifications-notify
                 :title (xml-escape-string nick)
                 :body (xml-escape-string msg)
                 :replaces-id (if (equal (ring-length erc-notifications-ring)
                                         erc-notifications-ring-size)
                                  (ring-remove erc-notifications-ring))
                 :app-icon erc-notifications-icon)))))))

(erc-notifications-mode t)

(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))

(setq erc-join-buffer 'bury)

(defun erc-start-or-switch (arg)
  "Connect to ERC, or switch to last active buffer"
  (interactive "P")
  (if (get-buffer "irc.freenode.net:6667") ;; ERC already active?
      (erc-track-switch-buffer 1) ;; yes: switch to last active
    (when (or arg (y-or-n-p "Start ERC? ")) ;; no: maybe start ERC
      (erc :server "irc.freenode.net"
           :port 6667
           :nick "thisirs"
           :password (secrets-get-secret "Default" "NickServ")))))

(setq erc-pcomplete-order-nickname-completions t)

(defun my-erc-mode-hook ()
  (set (make-local-variable 'scroll-conservatively) 101)
  (set (make-local-variable 'scroll-step) 1))

(add-hook 'erc-mode-hook 'my-erc-mode-hook)

(setq erc-format-nick-function 'erc-format-@nick)

(provide 'init-erc)
