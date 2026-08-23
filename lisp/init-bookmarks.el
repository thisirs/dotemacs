;;; init-bookmarks.el --- -*- lexical-binding: t; -*-

;; Browse the Firefox bookmarks dumped to `my/bookmark-file'.
;;
;; Candidates are three padded columns (title, host, tags) followed by an
;; invisible copy of the untruncated href/title/tags.  Emacs matches against
;; the candidate string itself, so that hidden suffix is what makes tags and
;; full URL paths searchable; annotation-/affixation-function cannot do it.

(require 'cl-lib)
(require 'url-parse)
(require 'mule-util)

(defvar my/bookmark-file "~/.mozilla/firefox/bookmark_list.el"
  "File holding the Firefox bookmark dump, as a readable alist.")

(defvar my/bookmark-host-fallback-width 24
  "Max width of the fallback shown for host-less URLs (about:, file:, ...).")

(defvar my/bookmark-column-percentile 90
  "Fit columns to this percentile of content width; wider values are ellipsized.
Sizing to the maximum lets a single outlier pad every row.")

(defvar my/bookmark--cache nil
  "Cons of (MTIME . ENTRIES).")

(defvar my/bookmark--display-cache nil
  "Cons of ((MTIME WIDTH) . CANDIDATES).")

(defun my/bookmark--host (href)
  "Short display host for HREF: no scheme, no userinfo, no port, no leading www.
For schemes without a host (about:, file:, ...) falls back to a truncated HREF."
  (let* ((parsed (ignore-errors (url-generic-parse-url href)))
         (host (and parsed (url-host parsed))))
    (if (and host (not (string-empty-p host)))
        (replace-regexp-in-string "\\`www\\." "" host)
      (truncate-string-to-width href my/bookmark-host-fallback-width nil nil t))))

(defun my/bookmark--mtime ()
  "Modification time of `my/bookmark-file', or signal a readable error."
  (or (file-attribute-modification-time
       (file-attributes (expand-file-name my/bookmark-file)))
      (user-error "No bookmark file at %s" my/bookmark-file)))

(defun my/bookmark--entries ()
  "Return cached list of (HREF TITLE TAGS HOST), re-parsing only on file change."
  (let ((mtime (my/bookmark--mtime)))
    (unless (equal mtime (car my/bookmark--cache))
      (setq my/bookmark--cache
            (cons mtime
                  (cl-loop
                   for (_ . fields) in (with-temp-buffer
                                         (insert-file-contents my/bookmark-file)
                                         (goto-char (point-min))
                                         (read (current-buffer)))
                   for href = (or (cdr (assoc "href" fields)) "")
                   unless (string-empty-p href)
                   collect (list href
                                 (or (cdr (assoc "title" fields)) "")
                                 (or (cdr (assoc "tags" fields)) "")
                                 (my/bookmark--host href))))))
    (cdr my/bookmark--cache)))

(defun my/bookmark--fit (strings cap)
  "Width fitting `my/bookmark-column-percentile' of non-empty STRINGS, at most CAP.
Returns 0 when nothing is non-empty, so the column can be dropped entirely."
  (let* ((ws (sort (cl-loop for s in strings
                            for w = (string-width s)
                            unless (zerop w) collect w)
                   #'<))
         (n (length ws)))
    (if (zerop n)
        0
      (min cap (nth (min (1- n)
                         (floor (* n (/ my/bookmark-column-percentile 100.0))))
                    ws)))))

(defun my/bookmark-candidates ()
  "Return an alist of (DISPLAY . HREF) sized to the current frame."
  (let* ((mtime (my/bookmark--mtime))
         (width (1- (frame-width)))
         (key (list mtime width)))
    (unless (equal key (car my/bookmark--display-cache))
      (let* ((entries (my/bookmark--entries))
             (avail (- width 2))
             (w2 (my/bookmark--fit (mapcar (lambda (e) (nth 3 e)) entries)
                                   (floor (* avail 0.30))))
             (w3 (my/bookmark--fit (mapcar (lambda (e) (nth 2 e)) entries)
                                   (floor (* avail 0.25))))
             (w1 (- avail w2 w3)))     ; title takes whatever is left
        (setq my/bookmark--display-cache
              (cons key
                    (cl-loop
                     for (href title tags host) in entries
                     collect
                     (cons (concat
                            (string-join
                             (delq nil
                                   (list (truncate-string-to-width title w1 0 ?\s)
                                         (unless (zerop w2)
                                           (truncate-string-to-width host w2 0 ?\s))
                                         (unless (zerop w3)
                                           (truncate-string-to-width tags w3 0 ?\s))))
                             " ")
                            " "
                            (propertize (mapconcat #'identity (list href title tags) " ")
                                        'invisible t))
                           href))))))
    (cdr my/bookmark--display-cache)))

(defun my/bookmark-invalidate-cache ()
  "Force the next `my/browse-bookmark' to re-read and re-format everything."
  (interactive)
  (setq my/bookmark--cache nil
        my/bookmark--display-cache nil))

(defun my/browse-bookmark ()
  "Pick a Firefox bookmark and open it in a browser."
  (interactive)
  (let* ((candidates (my/bookmark-candidates))
         (choice (progn
                   (unless candidates
                     (user-error "No bookmarks in %s" my/bookmark-file))
                   (completing-read
                    "Bookmark: "
                    (lambda (string predicate action)
                      (if (eq action 'metadata)
                          `(metadata
                            (display-sort-function . ,#'identity)
                            (cycle-sort-function   . ,#'identity)
                            (category . bookmark-url))
                        (complete-with-action action candidates string predicate)))
                    nil t))))
    (browse-url (cdr (assoc choice candidates)))))

(keymap-global-set "C-c j" #'my/browse-bookmark)

(provide 'init-bookmarks)
