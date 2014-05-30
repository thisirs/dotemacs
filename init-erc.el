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
                    (push (list name "fr") erc-spelling-dictionaries)
                  (push (list name "en") erc-spelling-dictionaries)))
              (cdr chan)))
      erc-autojoin-channels-alist)

;; Custom prompt
(setq erc-prompt
      (lambda () (concat (buffer-name) ">")))

(erc-match-mode 1)
(setq erc-keywords '("magit" "koans" "rubywarrior" "org"))

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
  (if (and (get-buffer "irc.freenode.net:6667")
           (erc-server-process-alive (get-buffer "irc.freenode.net:6667")))
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

;; Same color message
(require 'ring)

(define-erc-module colorize nil
  "This module highlights messages of a user with the same face."
  ((add-hook 'erc-insert-modify-hook 'erc-colorize-message 'append)
   (add-hook 'erc-mode-hook 'erc-colorize-setup)
   (erc-buffer-list #'erc-colorize-setup))
  ((remove-hook 'erc-insert-modify-hook 'erc-colorize-message)
   (remove-hook 'erc-mode-hook 'erc-colorize-setup)))

(defun erc-colorize-setup ()
  "Initialize nickname vs face ring."
  (setq erc-colorize-ring (make-ring (length erc-colorize-faces))))

(defvar erc-colorize-faces
  '(org-level-1
    org-level-2
    org-level-3
    org-level-4
    org-level-5
    org-level-6
    org-level-7
    org-level-8)
  "List of faces to apply to users' messages.")

(defvar erc-colorize-ring nil "Ring of conses of the form (NICK . FACE).")
(make-variable-buffer-local 'erc-colorize-ring)

(defun ring-assoc (ring nickname)
  "Return index of conses in RING whose car is NICKNAME, else nil."
  (catch 'found
    (dotimes (ind (ring-length ring) nil)
      (when (equal nickname (car (ring-ref ring ind)))
        (throw 'found ind)))))

(defun erc-colorize-color (nickname)
  "Return the face used for NICKNAME.

First look up `erc-colorize-ring' if there is already an
association. If not, pick the first face in `erc-colorize-faces'
that is not already used. If none, take the face of the least
active user."
  (let ((ind (ring-assoc erc-colorize-ring nickname)))
    (if ind
        (progn
          (let ((last (ring-remove erc-colorize-ring ind)))
            (ring-insert erc-colorize-ring last))
          (cdr (ring-ref erc-colorize-ring 0)))
      (let* ((used (mapcar #'cdr (ring-elements erc-colorize-ring)))
             (face (catch 'found
                     (dolist (f erc-colorize-faces)
                       (unless (member f used)
                         (throw 'found f))))))
        (if face
            (progn
              (ring-insert erc-colorize-ring (cons nickname face))
              face)
          (let ((older (ring-remove erc-colorize-ring)))
            (ring-insert erc-colorize-ring (cons nickname (cdr older)))
            (cdr older)))))))

(defun erc-colorize-message ()
  "Function used in `erc-insert-modify-hook' to apply the same face to a
message coming from a user."
  (let* ((vector (erc-get-parsed-vector (point-min)))
         (nickuserhost (erc-get-parsed-vector-nick vector))
         (nickname (and nickuserhost
                        (nth 0 (erc-parse-user nickuserhost))))
         (match-face (erc-colorize-color nickname)))
    (erc-put-text-property
     (point-min) (point-max)
     'font-lock-face match-face)))

(erc-readonly-mode -1)
(erc-colorize-mode 1)

(provide 'init-erc)
