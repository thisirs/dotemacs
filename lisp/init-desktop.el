;;; desktop-mode
;; save a list of open files in ~/.emacs.desktop

(setq desktop-load-locked-desktop nil)

;; save the desktop file automatically if it already exists
(setq desktop-save 'if-exists)
(desktop-save-mode 1)

;; save a bunch of variables to the desktop file
;; for lists specify the len of the maximal saved data also
(setq desktop-globals-to-save
      '((extended-command-history . 30)
        (file-name-history        . 100)
        (grep-history             . 30)
        (compile-history          . 30)
        (minibuffer-history       . 50)
        (query-replace-history    . 60)
        (read-expression-history  . 60)
        (regexp-history           . 60)
        (regexp-search-ring       . 20)
        (search-ring              . 20)
        (shell-command-history    . 50)
        tags-file-name
        register-alist))

(setq desktop-files-not-to-save
      "\\(^/[^/:]*:\\|(ftp)$\\)\\|\\(^/tmp/\\)\\|\\(.gpg$\\)\\|\\(.el.gz$\\)")
(setq desktop-buffers-not-to-save
      (concat "\\(" "^nn\\.a[0-9]+\\|\\.log\\|(ftp)"
              "\\)$"))

(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
(add-to-list 'desktop-modes-not-to-save 'DocView-mode)

;; ;; buffer-display-time is changed when desktop is loaded
;; (add-to-list 'desktop-locals-to-save 'buffer-display-time-1)

;; (make-variable-buffer-local 'buffer-display-time-1)

;; (defun save-buffer-display-time ()
;;   (mapc (lambda (buf)
;;           (with-current-buffer buf
;;             (setq buffer-display-time-1
;;                   (or buffer-display-time (current-time)))))
;;         (buffer-list)))

;; (add-hook 'desktop-save-hook #'save-buffer-display-time)

;; (defun set-buffer-display-time ()
;;   (mapc (lambda (buf)
;;           (with-current-buffer buf
;;             (setq buffer-display-time buffer-display-time-1)))
;;         (buffer-list)))

;; (add-hook 'desktop-after-read-hook #'set-buffer-display-time)

(defun emacs-process-p (pid)
  "Return non-nil if PID is the process id of an emacs process, else return nil."
  (when (integerp pid)
    (let* ((cmdline-file (concat "/proc/" (int-to-string pid) "/cmdline")))
      (when (file-exists-p cmdline-file)
        (with-temp-buffer
          (insert-file-contents-literally cmdline-file)
          (goto-char (point-min))
          (and (search-forward "emacs" nil t) t))))))

(defadvice desktop-owner (after pry-from-cold-dead-hands activate)
  "Don't allow dead emacsen to own the desktop file."
  (when (not (emacs-process-p ad-return-value))
    (setq ad-return-value nil)))


(provide 'init-desktop)
