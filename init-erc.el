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
         "#linux-fr"
         "#debianfr"
         "#TikZ"
         "#org-mode-fr")))

(erc-match-mode 1)
(setq erc-keywords '("magit" "koans" "rubywarrior" " org" "?"))

;; Custom prompt
(setq erc-prompt
      (lambda () (concat (buffer-name) ">")))

(defun my-notify-erc (match-type nickuserhost message)
  "Notify when a message is received."
  (notifications-notify
   :title (format "%s in %s"
                  ;; Username of sender
                  (car (split-string nickuserhost "!"))
                  ;; Channel
                  (or (erc-default-target) "#unknown"))
   :body (cond
          ((eq match-type 'current-nick)
           (if (string-match "^[Tt]hisirs" message)
               "is talking to you!"
             "is talking about you!"))
          ((and (eq match-type 'keywords)
                (string-match "?" message))
           (and (string-match "?$" message)
                (concat "is asking a question!\n" message)))
          (t
           (replace-regexp-in-string "[\t\n ]+" " " message)))
   :icon "emacs-snapshot"
   :timeout -1))

(add-hook 'erc-text-matched-hook 'my-notify-erc)

(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))

(setq erc-join-buffer 'bury)

(defun erc-start-or-switch (arg)
  "Connect to ERC, or switch to last active buffer"
  (interactive "P")
  (if (get-buffer "irc.freenode.net:6667") ;; ERC already active?
      (erc-track-switch-buffer 1) ;; yes: switch to last active
    (when (or arg (y-or-n-p "Start ERC? ")) ;; no: maybe start ERC
      (erc :server "irc.freenode.net" :port 6667 :nick "thisirs"))))

(setq  erc-pcomplete-order-nickname-completions t)

(defun my-erc-mode-hook ()
  (set (make-local-variable 'scroll-conservatively) 101)
  (set (make-local-variable 'scroll-step) 1))

(add-hook 'erc-mode-hook 'my-erc-mode-hook)

(provide 'init-erc)
