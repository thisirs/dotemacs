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

;; gnome-open file that emacs can't
(defun gnome-open (filename)
  (let ((process-connection-type nil))
    (start-process "" nil "/usr/bin/gnome-open"
                   (expand-file-name filename))))

(defadvice find-file (around find-or-launch-file)
  "Gnome opens file that emacs can't."
  (cond
   ((string-match
     (concat
      "\\."
      (regexp-opt '("ods" "odt" "pdf" "doc") t)
      "$")
     (ad-get-arg 0))
    (gnome-open (ad-get-arg 0))
    (message "Gnome-opening file..."))
   (t
    ad-do-it)))

(ad-activate 'find-file)

(provide 'init-find-file)
