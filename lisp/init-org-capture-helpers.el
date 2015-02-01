(defun org-capture-inline-todo ()
  "Return an org link pointing to the location we captured from
if a TODO cookie is present on the line."
  (or
   (with-current-buffer (org-capture-get :original-buffer)
     (and (buffer-file-name)
          (save-excursion
            (beginning-of-line)
            (when (looking-at
                   (concat "[\t ]*"
                           (regexp-quote (or comment-start ""))
                           "[\t ]+TODO:?[\t ]+"))
              (goto-char (match-end 0))
              (let* ((txt (buffer-substring (point) (line-end-position)))
                     (search (org-make-org-heading-search-string
                              (buffer-substring (line-beginning-position)
                                                (line-end-position))))
                     (link (concat "file:" (abbreviate-file-name buffer-file-name)
                                   "::" search)))
                (org-make-link-string link txt))))))
   ""))

;; Return stored link with description "here"
(defun my-name ()
  (file-name-nondirectory
   (replace-regexp-in-string
    "%" "\\%"
    (org-link-unescape (or (plist-get org-store-link-plist :link) "")))))

(defun org-capture-fill-region ()
  "Return the selected region at the time of the capture."
  (with-temp-buffer
    (insert (or (plist-get org-store-link-plist :initial) ""))
    (fill-region (buffer-end 0) (buffer-end 1))
    (buffer-string)))

(defun org-capture-timestamp-from-now (days &optional before)
  "Return an active DAYS days from now org time-stamp."
  (let ((time (format-time-string
               (car org-time-stamp-formats)
               (time-add (current-time) (days-to-time days)))))
    (if (integerp before)
        (concat (substring time 0 -1) " -" (format "%d" before) "d>")
      time)))

(provide 'init-org-capture-helpers)
