;; Open a buffer with sudo via tramp
(defun th-find-file-sudo (file)
  "Opens FILE with root privileges."
  (interactive "F")
  (set-buffer
   (find-file
    (concat "/sudo::"
            (expand-file-name file)))))

(defadvice find-file (around th-find-file activate)
  "Open FILENAME using tramp's sudo method if it's read-only."
  (if (and (file-exists-p (ad-get-arg 0))
           (not (file-writable-p (ad-get-arg 0)))
           (not (file-remote-p (ad-get-arg 0)))
           (not (file-directory-p (ad-get-arg 0)))
           (y-or-n-p (concat "File "
                             (ad-get-arg 0)
                             " is read-only.  Open it as root? ")))
      (th-find-file-sudo (ad-get-arg 0))
    ad-do-it))

(defadvice find-file (around find-or-launch-file activate)
  "Org open file that emacs can't."
  (cond
   ((string-match
     (concat
      "\\."
      (regexp-opt '("ods" "odt" "pdf" "docx" "doc" "xls" "xlsx" "avi" "mp4") t)
      "$")
     (ad-get-arg 0))
    (org-open-file (ad-get-arg 0))
    (message "Opening file..."))
   (t
    ad-do-it)))

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
