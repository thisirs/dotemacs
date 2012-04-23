(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/emacs-helm"))

(setq helm-command-prefix-key "C-x C-a")

(require 'helm-config)

;; (helm-read-string-mode 1)
;; (ac-mode -1)

(setq helm-su-or-sudo "sudo")

(setq helm-c-locate-command "locate -e -b -i -r \"%s\"")

;; don't save history information to file
(remove-hook 'kill-emacs-hook 'helm-c-adaptive-save-history)

;; make `helm-for-files-prefered-list' dynamic
(defadvice helm-for-files (before update-helm-list activate)
  (setq helm-for-files-prefered-list
        (helm-for-files-update-list)))

(defun helm-for-files-update-list ()
  `(helm-c-source-ffap-line
    helm-c-source-ffap-guesser
    helm-c-source-buffers-list
    helm-c-source-recentf
    helm-c-source-bookmarks
    helm-c-source-file-cache
    helm-c-source-files-in-current-dir
    ,@(if (file-exists-p "/media/THISKEY") '(helm-c-source-locate-thiskey))
    helm-c-source-locate))

(defun helm-c-locate-thiskey-init ()
  "Initialize async locate process for `helm-c-source-locate'."
  (start-process-shell-command
   "locate-thiskey-process" nil
   (format (concat "locate -e -b -d " (expand-file-name "~/.locate.db") " -i -r \"%s\"")
           helm-pattern)))

(defvar helm-c-source-locate-thiskey
  '((name . "Locate in THISKEY")
    (candidates . helm-c-locate-thiskey-init)
    (type . file)
    (requires-pattern . 3)
    (delayed))
  "Find files matching the current input pattern with locate.")

(provide 'init-helm)