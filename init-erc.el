;;; erc
;; check channels
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
         "#emacsfr"
         "#linux-fr"
         "#debianfr"
         "#TikZ"
         "#org-mode-fr")))

(erc-match-mode 1)
(setq erc-keywords '("magit" "koans" "rubywarrior" " org" "?"))

;; custom prompt
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

(defun erc-start-or-switch ()
  "Connect to ERC, or switch to last active buffer"
  (interactive)
  (if (get-buffer "irc.freenode.net:6667") ;; ERC already active?
      (erc-track-switch-buffer 1) ;; yes: switch to last active
    (when (y-or-n-p "Start ERC? ") ;; no: maybe start ERC
      (erc :server "irc.freenode.net" :port 6667 :nick "thisirs"))))

(defun pcomplete-erc-command-name ()
  "Returns the command name of the first argument."
  (let ((cmd (pcomplete-arg 'first)))
    (cond
     ((member (substring cmd 0 -1)
              (pcomplete-erc-nicks))
      "NICKLIST")
     ((eq (elt cmd 0) ?/)
      (upcase (substring cmd 1)))
     (t "SAY"))))

(defun is-nick-p (nick)
  (member (substring nick 0 -1)
          (pcomplete-erc-nicks)))

(defun pcomplete/erc-mode/NICKLIST ()
  (while (and (pcomplete-test 'is-nick-p)
              (or (= pcomplete-index pcomplete-last) (pcomplete-test 'is-nick-p 0)))
    (let ((start erc-input-marker))
      (save-excursion
        (goto-char (pcomplete-begin 0))
        (while (re-search-backward ": " start t)
          (replace-match ", "))))
    (pcomplete-here (pcomplete-erc-nicks ": ")))
  (while (pcomplete-here (pcomplete-erc-nicks))))

(provide 'init-erc)
