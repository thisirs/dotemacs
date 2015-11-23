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

;; Open certain files externally with ido, dired, find-file
(defvar find-file-externally '("ods" "odt" "pdf" "docx" "doc" "xls" "xlsx" "avi" "mp4")
  "List of file extensions that should be opened externally.")

(defun find-file-noselect-org-open (oldfun filename &optional nowarn rawfile wildcards)
  "Advice on `find-file-noselect' to open files externally."
  (if (string-match (concat "\\." (regexp-opt find-file-externally t) "$") filename)
      (prog1 nil
        (org-open-file filename)
        (message "Opening file externally"))
    (funcall oldfun filename nowarn rawfile wildcards)))

(advice-add 'find-file-noselect :around #'find-file-noselect-org-open)

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
