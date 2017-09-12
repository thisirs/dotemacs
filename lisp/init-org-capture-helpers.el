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
    (fill-region (point-min) (point-max))
    (buffer-string)))

(defun org-capture-timestamp-from-now (days &optional before)
  "Return an active org time-stamp that is DAYS ahead from now.
If BEFORE is an integer, add a warn time."
  (let ((time (format-time-string
               (car org-time-stamp-formats)
               (time-add (current-time) (days-to-time days)))))
    (if (integerp before)
        (concat (substring time 0 -1) " -" (format "%d" before) "d>")
      time)))

(defun org-capture-read-date ()
  "To be used in a capture template as %(org-capture-read-date). It reads"
  (let ((obuf (org-capture-get :original-buffer)) ts time)
    (when (bufferp obuf)
      (with-current-buffer obuf
        (when (eq major-mode 'org-agenda-mode)
          (when-let ((day (org-get-at-bol 'day))
                     (mdy (calendar-gregorian-from-absolute day)))
            (setq time (org-get-at-bol 'time-of-day))
            (setq ts (encode-time
                      0
                      (if time (mod time 100) 0)
                      (if time (floor (/ time 100)) 0)
                      (nth 1 mdy)
                      (nth 0 mdy)
                      (nth 2 mdy)))))))
    (org-insert-time-stamp (org-read-date (not (null time)) t nil nil ts)
                           (and (boundp 'org-time-was-given) org-time-was-given))))

(defvar org-tags-to-report-alist
  '(("ensei" . "~/CloudStation/Sylvain/enseignements/notebook.org")
    ("rech" . "~/CloudStation/Sylvain/recherche/notebook.org")))

(defun org-agenda-add-report (&optional delete-other-windows)
  "Visit file from current agenda line.

It looks at the current line of the org agenda and find a file
from the tags of the event. The mapping is controlled by the
variable `org-tags-to-report-alist'.

Look for an appointement in current org agenda line and go to
appropriate report file."
  (interactive "P")
  (unless (eq major-mode 'org-agenda-mode)
    (error "Not in an org agenda"))
  (let* ((txt (org-no-properties (org-get-at-bol 'txt)))
         (tags-list (org-get-at-bol 'tags))
         (day (org-get-at-bol 'day))
         (mdy (calendar-gregorian-from-absolute day))
         (time (org-get-at-bol 'time-of-day))
         (ts (encode-time 0 (if time (mod time 100) 0) (if time (floor (/ time 100)) 0)
                          (nth 1 mdy)
                          (nth 0 mdy)
                          (nth 2 mdy)))
         (report (assoc-default tags-list org-tags-to-report-alist
                                (lambda (e k)
                                  (eval (cdr (let (todo-only)
                                               (org-make-tags-matcher e))))))))
    (if report
        (let ((buffer (find-file-noselect report)))
          (pop-to-buffer-same-window buffer)
          (when delete-other-windows (delete-other-windows))
          (widen)
          (goto-char (point-max))
          (or (bolp) (insert "\n"))
          (insert "* ")
          (org-insert-time-stamp ts t t)
          (insert " " txt "\n"))
      (user-error "No corresponding report file found"))))

(defun org-capture--org-set-tags ()
  "Make `org-set-tags' available from embedded lisp in capture
templates. Use %(org-capture--org-set-tags) for interactive tags
insertion."
  (let ((org-inhibit-startup t)) (org-mode))
  (org-clone-local-variables (org-capture-get :original-buffer) "\\`org-")
  (org-set-tags))

(defun org-attach-attach-from (file)
  "Attach FILE to current entry.

Attachment directory is specified in property :attachments_dir in
capture template. When called interactively, offer a list of
recently downloaded files to attach."
  (interactive (list (completing-read
                      "File to attach: "
                      (let ((output (string-trim (shell-command-to-string "find ~/deathrow /tmp/mozilla_sylvain0 -maxdepth 1 -type f -exec ls -1t \"{}\" +;"))))
                        (unless (string-empty-p output)
                          (split-string output "\n"))))))
  (require 'org-attach)
  (let ((org-attach-directory (org-capture-get :attachments_dir)))
    (ignore (org-attach-attach file nil 'cp))))


(provide 'init-org-capture-helpers)
