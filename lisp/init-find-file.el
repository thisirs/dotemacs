;; Open a buffer with sudo via tramp
(defun find-file-sudo (oldfun filename &optional wildcards)
  (if (and (file-exists-p filename)
           (not (file-writable-p filename))
           (not (file-remote-p filename))
           (not (file-directory-p filename))
           (y-or-n-p (format "File %s is read-only.  Open it as root? " filename)))
      (find-file (concat "/sudo::" (expand-file-name filename)))
    (funcall oldfun filename wildcards)))

(advice-add 'find-file :around #'find-file-sudo)

(defun find-file-org-open (oldfun filename &optional wildcards)
  (let ((ex-list '("ods" "odt" "pdf" "docx" "doc" "xls" "xlsx" "avi" "mp4")))
    (if (not (string-match (concat "\\." (regexp-opt ex-list t) "$") filename))
        (funcall oldfun filename wildcards)
      (org-open-file filename)
      (message "Opening file..."))))

(advice-add 'find-file :around #'find-file-org-open)

(defun revert-all ()
  "Revert all buffers without asking."
  (interactive)
  (let ((bl (buffer-list)) buf)
    (while (setq buf (pop bl))
      (condition-case nil
          (with-current-buffer buf
            (let ((revert-without-query '(".")))
              (revert-buffer t)))))))

;; Taken from http://iqbalansari.github.io/blog/2014/12/07/automatically-create-parent-directories-on-visiting-a-new-file-in-emacs/
(defun my-create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions #'my-create-non-existent-directory)

(provide 'init-find-file)
