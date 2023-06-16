(use-package erc
  :elpaca nil
  :custom
  (erc-autojoin-channels-alist
   '(("libera.chat"
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
  (erc-autojoin-timing 'ident)
  (erc-prompt (lambda () (concat (buffer-name) ">"))))


;;; erc
;; Check channels
(use-package erc-track
  :elpaca nil
  :custom (erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                     "324" "329" "332" "333" "353" "477"))
  :config (erc-track-mode 1))

(use-package erc-spelling
  :elpaca nil
  :config (erc-spelling-mode 1))

;; Flyspell input line with different dictionaries
;; (mapc (lambda (chan)
;;         (mapc (lambda (name)
;;                 (if (string-match "fr$" name)
;;                     (push (list name "francais") erc-spelling-dictionaries)
;;                   (push (list name "english") erc-spelling-dictionaries)))
;;               (cdr chan)))
;;       erc-autojoin-channels-alist)

;; (erc-match-mode 1)
;; (setq erc-keywords '("\\<magit\\>" "\\<koans\\>" "\\<rubywarrior\\>" "\\<org\\>"))

;; (defvar erc-notifications-ring-size 5
;;   "Maximum number of simultaneous notifications.")

;; (defvar erc-notifications-ring (make-ring (1+ erc-notifications-ring-size))
;;   "Ring of notifications id.")

;; (with-eval-after-load 'erc-desktop-notifications
;;   (setq erc-notifications-icon "emacs-snapshot")
;;   (defun erc-notifications-notify-on-match (match-type nickuserhost msg)
;;     (let ((nick (nth 0 (erc-parse-user nickuserhost))))
;;       (unless (or (string-match-p "^Server:" nick)
;;                   (when (boundp 'erc-track-exclude)
;;                     (member nick erc-track-exclude)))
;;         (dbus-ignore-errors
;;           (ring-insert
;;            erc-notifications-ring
;;            (notifications-notify
;;             :title (xml-escape-string (concat nick " on " (or (erc-default-target) "#unknown")))
;;             :body (xml-escape-string (replace-regexp-in-string "[\t\n ]+" " " msg))
;;             :replaces-id (if (equal (ring-length erc-notifications-ring)
;;                                     erc-notifications-ring-size)
;;                              (ring-remove erc-notifications-ring))
;;             :app-icon erc-notifications-icon)))))))

;; (erc-notifications-mode t)

(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))

(setq erc-join-buffer 'bury)

(defun erc-start-or-switch (arg)
  "Switch to the last ERC active buffer.
Start ERC if it is not running and ask for confirmation if ARG is
nil."
  (interactive "P")
  (if (and (get-buffer "irc.libera.chat:6667")
           (erc-server-process-alive (get-buffer "irc.libera.chat:6667")))
      (erc-track-switch-buffer 1) ;; yes: switch to last active
    (when (or arg (y-or-n-p "Start ERC? ")) ;; no: maybe start ERC
      (let ((results (auth-source-search :max 1
                                         :host "NickServ"
                                         :require '(:host))))
        (erc :server "irc.libera.chat"
             :port "6667"
             :nick "thisirs"
             :password (if-let* ((it (funcall (plist-get (car results) :secret)))) it "")))
      (erc-track-switch-buffer 1))))

(setq erc-pcomplete-order-nickname-completions t)

(defun my-erc-mode-hook ()
  (whitespace-mode -1)
  (set (make-local-variable 'scroll-conservatively) 101)
  (set (make-local-variable 'scroll-step) 1))

(add-hook 'erc-mode-hook #'my-erc-mode-hook)

(setq erc-format-nick-function 'erc-format-@nick)

;; Per user message colorization
;; https://github.com/thisirs/erc-colorize.git
(use-package erc-colorize               ; Per user colorization of whole message
  :config
  (erc-colorize-mode 1))

(use-package erc-fill
  :elpaca nil
  :custom
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 20)
  (erc-fill-column 102))

(provide 'init-erc)
