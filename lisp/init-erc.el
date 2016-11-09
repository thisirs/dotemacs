;;; erc
;; Check channels
(erc-track-mode t)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477"))

;; List of channels to join at startup
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
                    (push (list name "francais") erc-spelling-dictionaries)
                  (push (list name "english") erc-spelling-dictionaries)))
              (cdr chan)))
      erc-autojoin-channels-alist)

;; Custom prompt
(setq erc-prompt
      (lambda () (concat (buffer-name) ">")))

(erc-match-mode 1)
(setq erc-keywords '("\\<magit\\>" "\\<koans\\>" "\\<rubywarrior\\>" "\\<org\\>"))

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
          (ring-insert
           erc-notifications-ring
           (notifications-notify
            :title (xml-escape-string (concat nick " on " (or (erc-default-target) "#unknown")))
            :body (xml-escape-string (replace-regexp-in-string "[\t\n ]+" " " msg))
            :replaces-id (if (equal (ring-length erc-notifications-ring)
                                    erc-notifications-ring-size)
                             (ring-remove erc-notifications-ring))
            :app-icon erc-notifications-icon)))))))

(erc-notifications-mode t)

(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))

(setq erc-join-buffer 'bury)

(defun erc-start-or-switch (arg)
  "Switch to the last ERC active buffer.
Start ERC if it is not running and ask for confirmation if ARG is
nil."
  (interactive "P")
  (if (and (get-buffer "irc.freenode.net:6667")
           (erc-server-process-alive (get-buffer "irc.freenode.net:6667")))
      (erc-track-switch-buffer 1) ;; yes: switch to last active
    (when (or arg (y-or-n-p "Start ERC? ")) ;; no: maybe start ERC
      (erc :server "irc.freenode.net"
           :port "6667"
           :nick "thisirs"
           :password (if-let ((it (funcall (plist-get (car (auth-source-search :max 1
                                                                               :host "NickServ"
                                                                               :require '(:host)))
                                                      :secret))))
                         it ""))
      (erc-track-switch-buffer 1))))

(setq erc-pcomplete-order-nickname-completions t)

(defun my-erc-mode-hook ()
  (whitespace-mode -1)
  (set (make-local-variable 'scroll-conservatively) 101)
  (set (make-local-variable 'scroll-step) 1))

(add-hook 'erc-mode-hook #'my-erc-mode-hook)

(setq erc-format-nick-function 'erc-format-@nick)

(erc-readonly-mode -1)

;; Per user message colorization
(use-package erc-colorize
  :ensure
  :config
  (erc-colorize-mode 1))

;; Make nicks aligned to the right
(setq erc-fill-function 'erc-fill-static)
(setq erc-fill-static-center 20)
(setq erc-fill-column 102)

(provide 'init-erc)
