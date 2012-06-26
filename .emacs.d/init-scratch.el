;; auto-save with non-visiting buffer is too rigid
(defun save-scratch-buffer ()
  "Create a backup of scratch buffer"
  (and (get-buffer "*scratch*")
       (with-current-buffer "*scratch*"
         (and (buffer-modified-p)
              (write-file
               (concat (file-name-as-directory
                        (assoc-default "*scratch*" backup-directory-alist 'string-match))
                       "scratch-buffer-backup.el"))))))

(add-hook 'kill-emacs-hook 'save-scratch-buffer)

;; put something different in the scratch buffer
(setq initial-scratch-message
      ";; scratch buffer created -- happy hacking\n\n")

(defun insert-in-scratch (string)
  "Insert STRING in the scratch buffer. It is commented out and
filled."
  (with-current-buffer (get-buffer-create "*scratch*")
    (goto-char (point-max))
    (or (bolp) (insert "\n"))
    (insert
     (with-temp-buffer
       (insert (mapconcat
                'identity
                (split-string string "\n+")
                " "))
       (let ((fill-colunm 70)
             (fill-prefix ";; "))
         (goto-char (point-min))
         (insert ";; ")
         (fill-region (point-min) (point-max)))
       (buffer-string))
     "\n\n")
    (set-buffer-modified-p nil)))

(defun insert-SCMB-in-scratch ()
  (and (executable-find "ruby")
       (set-process-filter
        (start-process-shell-command
         "msg in scratch buffer"
         nil
         "ruby ~/Dropbox/scripts/SCMB.rb")
        (lambda (process string)
          (insert-in-scratch string)))))

(insert-SCMB-in-scratch)

(provide 'init-scratch)
