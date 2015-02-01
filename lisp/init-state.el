;; Quick navigation between workspaces
(if (file-exists-p "~/CloudStation/Sylvain/emacs/site-lisp/state/state.el")
    (load-file "~/CloudStation/Sylvain/emacs/site-lisp/state/state.el")
  (require 'state))

(state-define-state
 debug
 :key "d"
 :switch "*debug*")

(state-define-state
 gnus
 :key "g"
 :in (memq major-mode
           '(message-mode
             gnus-group-mode
             gnus-summary-mode
             gnus-article-mode))
 :create gnus)

(state-define-state
 erc
 :key "i"
 :in (and (fboundp 'erc-buffer-list)
          (memq (current-buffer) (erc-buffer-list)))
 :switch (erc-start-or-switch 1)
 :keep (erc-track-switch-buffer 0))

(state-define-state
 message
 :key "m"
 :switch "*Messages*")

(state-define-state
 scratch
 :key "s"
 :switch "*scratch*")

(state-define-state
 twit
 :key "t"
 :in (and (require 'twittering-mode nil t) (twittering-buffer-p))
 :switch twit)

(state-define-state
 programming_samples
 :key "r"
 :switch "~/CloudStation/Sylvain/Org/programming_samples.org")

(state-define-state
 personnal
 :key "p"
 :switch "~/CloudStation/Sylvain/Org/personnel.org.gpg")

(state-define-state
 emacs
 :key "e"
 :in "~/.emacs.d/init"
 :create (find-file "~/.emacs.d/init.el")
 :keep (unless (eq (current-buffer) (find-buffer-visiting "~/.emacs.d/init.el"))
         (find-file "~/.emacs.d/init.el")))

;; Bound state: only accessible from emacs state
(state-define-state
 emacs-term
 :key "z"
 :bound emacs
 :exist (get-buffer "*ansi-term (dotemacs)*")
 :in (equal (buffer-name) "*ansi-term (dotemacs)*")
 :switch (if (get-buffer-window "*ansi-term (dotemacs)*")
             (select-window (get-buffer-window "*ansi-term (dotemacs)*"))
           (switch-to-buffer-other-window "*ansi-term (dotemacs)*"))
 :create (progn
           (switch-to-buffer-other-window (current-buffer))
           (ansi-term "/bin/zsh" "ansi-term (dotemacs)")))

;; Not bound state with same key
(state-define-state
 term
 :key "z"
 :exist (get-buffer "*ansi-term*")
 :in (equal (buffer-name) "*ansi-term*")
 :switch (if (get-buffer-window "*ansi-term*")
             (select-window (get-buffer-window "*ansi-term*"))
           (switch-to-buffer-other-window "*ansi-term*"))
 :create (progn
           (switch-to-buffer-other-window (current-buffer))
           (ansi-term "/bin/zsh")))

;; Common pattern when defining a repl state
(defmacro state-define-repl (name key buffer-name from create)
  `(state-define-state
    ,name
    :bound ,from
    :key ,key
    :exist (get-buffer ,buffer-name)
    :in (equal (buffer-name) ,buffer-name)
    :switch (if (get-buffer-window ,buffer-name)
                (select-window (get-buffer-window ,buffer-name))
              (switch-to-buffer-other-window ,buffer-name))
    :create (progn
              (switch-to-buffer-other-window (current-buffer))
              ,create)))

(state-define-repl elisp-repl "j" "*ielm*" (eq major-mode 'emacs-lisp-mode) (ielm))
(state-define-repl matlab-repl "j" "*MATLAB*" (eq major-mode 'matlab-mode) (matlab-shell))
(state-define-repl python-repl "j" "*Python*" (eq major-mode 'python-mode) (call-interactively 'run-python))
(state-define-repl ruby-repl "j" "*ruby*" (eq major-mode 'ruby-mode) (inf-ruby))

(state-global-mode 1)

(provide 'init-state)
