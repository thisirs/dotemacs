;; ouvre un buffer en sudo via tramp
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

(defadvice find-file (around find-file-other-window activate)
  (if current-prefix-arg
      (find-file-other-window (ad-get-arg 0))
    ad-do-it))

(defun find-temp-file (extension)
  "quick find file in /tmp"
  (interactive "sExtension: ")
  (cond
   ((equal (length extension) 0) (find-file (make-temp-file "foo")))
   ((memq ?. (string-to-list extension))
    (find-file (concat "/tmp/" extension)))
   (t (find-file (concat (make-temp-file "foo") "." extension)))))

(global-set-key (kbd "C-x C-t") 'find-temp-file)

(defadvice find-file (around find-or-launch-file activate)
  "Org open file that emacs can't."
  (cond
   ((string-match
     (concat
      "\\."
      (regexp-opt '("ods" "odt" "pdf" "doc") t)
      "$")
     (ad-get-arg 0))
    (org-open-file (ad-get-arg 0) 'system)
    (message "Opening file..."))
   (t
    ad-do-it)))

(provide 'init-find-file)
