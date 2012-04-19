(defun latex-delete-unreferenced-labels ()
  "Delete all occurences of a label that is not referenced in the
document."
  (interactive)
  (save-excursion
    (let (labels (count 0))
      (goto-char (point-min))
      (while (re-search-forward "\\\\\\(eq\\|page\\|[fvF]\\)?ref{\\([^\n\r%\\{}]+\\)}" nil t)
        (setq labels (cons (match-string-no-properties 1) labels)))
      (goto-char (point-min))
      (while (re-search-forward "\\\\label{\\([^\n\r%\\{}]+\\)}" nil t)
        (unless (member (match-string-no-properties 1) labels)
          (delete-region (match-beginning 0) (match-end 0))
          (setq count (+ 1 count))))
      (message "%s label%s deleted!"
               (if (= count 0) "No" (int-to-string count))
               (if (>= count 2) "s" "")))))


(defun enclosing-braces-at-point ()
  (and (thing-at-point-looking-at "{\\([^}]*\\)}")
       (buffer-substring-no-properties (match-beginning 1) (match-end 1))))

(defun latex-refactor-label (label new)
  "Rename a label and its references in a LaTeX document. Word at
point is suggested as the default label to replace. A message
shows you how many labels and refs have been replaced."
  (interactive
   (list (let ((tap (or (enclosing-braces-at-point) (thing-at-point 'word))))
           (read-string
            (format "Old label%s: " (if tap (concat " (" tap ")") ""))
            nil nil tap))
         (read-string "New label: ")))
  (save-excursion
    (save-restriction
      (and mark-active (narrow-to-region (region-beginning) (region-end)))
      (message
       (concat "\"%s\" -> \"%s\": "
               (mapconcat
                (lambda (args)
                  (goto-char (point-min))
                  (let ((n 0))
                    (while (re-search-forward
                            (concat "\\\\\\(?:" (car args) "\\){\\("
                                    (regexp-quote label) "\\)}") nil t)
                      (setq n (1+ n))
                      (replace-match new t t nil 1))
                    (format "%d %s%s" n (cdr args) (if (> n 1) "s" ""))))
                '(("label" . "label")
                  ("\\(?:eq\\|page\\|[fvF]\\)?ref" . "reference"))
                " and ")
               " replaced in %s!")
       label new (if mark-active "region" "buffer")))))

(defun latex-occur-ref-wo-tilde ()
  (interactive)
  (occur "[^~(](?\\\\\\(eq\\|page\\|[fvF]\\)?ref"))

(defun latex-occur-ref-with-parent ()
  (interactive)
  (occur "(\\\\ref{[^{]*})"))

(defun latex-rename-label-after-includegraphics ()
  "Refactor label when it is preceded by an includegraphics. The
  filename used in includegraphics becomes the new label. Create
  a label when non existing."
  (interactive)
  (save-excursion
    (goto-char 1)
    (while (re-search-forward "\\\\includegraphics\\(\\[[^]]+\\]\\)?{\\([^}]+\\)" nil t)
      ;; remove extension from filename
      (let ((newname ((lambda (filename)
                        (substring filename
                                   0
                                   (string-match
                                    (concat
                                     (regexp-opt '(".jpeg" ".png" ".pdf"))
                                     "$")
                                    filename)))
                      (match-string-no-properties 2))))
        (forward-line)
        ;; skip caption for correct numbering
        (if (looking-at "[ \t]*\\\\caption") (forward-line))
        (if (looking-at "[ \t]*\\\\label{\\([^}]+\\)")
            (unless (equal (match-string-no-properties 1) newname)
              (latex-refactor-label (match-string-no-properties 1) newname))
          (open-line 1)
          (insert (format "\\label{%s}" newname))
          (indent-for-tab-command))))))

(defun latex-replace-by-closest ()
  "Replace filename in all includegraphics by closest filename
with respect to Levenshtein distance. Candidates are found in the
subdirectory img and filtered by extension."
  (interactive)
  (let ((img-files (directory-files "./img" nil
                                    (regexp-opt '(".jpg" ".jpeg" ".png")))))
    (save-excursion
      (goto-char 1)
      (while (re-search-forward "\\\\includegraphics\\(\\[[^]]+\\]\\)?{\\([^}]+\\)" nil t)
        (let* ((old-filename (match-string-no-properties 2))
               (closest "")
               (new-filename (dolist (file img-files closest)
                               (if (> (levenshtein-distance old-filename closest)
                                      (levenshtein-distance old-filename
                                                            (file-name-sans-extension file)))
                                   (setq closest (file-name-sans-extension file))))))
          (and
           (> (length new-filename) 0)
           (not (equal new-filename old-filename))
           (yes-or-no-p
            (format "Replace %s by %s? " old-filename new-filename))
           (replace-match new-filename nil nil nil 2)))))))

;; taken from http://en.wikibooks.org/wiki/Algorithm_Implementation/Strings/Levenshtein_distance#Emacs_Lisp
(defun levenshtein-distance (str1 str2)
  "Return the edit distance between strings STR1 and STR2."
  (if (not (stringp str1))
      (error "Argument was not a string: %s" str1))
  (if (not (stringp str2))
      (error "Argument was not a string: %s" str2))
  (let* ((make-table (function (lambda (columns rows init)
                                 (make-vector rows (make-vector columns init)))))
         (tref (function (lambda (table x y)
                           (aref (aref table y) x))))
         (tset (function (lambda (table x y object)
                           (let ((row (copy-sequence (aref table y))))
                             (aset row x object)
                             (aset table y row) object))))
         (length-str1 (length str1))
         (length-str2 (length str2))
         (d (funcall make-table (1+ length-str1) (1+ length-str2) 0)))
    (let ((i 0) (j 0))
      (while (<= i length-str1)
        (funcall tset d i 0 i)
        (setq i (1+ i)))
      (while (<= j length-str2)
        (funcall tset d 0 j j)
        (setq j (1+ j))))
    (let ((i 1))
      (while (<= i length-str1)
        (let ((j 1))
          (while (<= j length-str2)
            (let* ((cost (if (equal (aref str1 (1- i)) (aref str2 (1- j)))
                             0
                           1))
                   (deletion (1+ (funcall tref d (1- i) j)))
                   (insertion (1+ (funcall tref d i (1- j))))
                   (substitution (+ (funcall tref d (1- i) (1- j)) cost)))
              (funcall tset d i j (min insertion deletion substitution)))
            (setq j (1+ j))))
        (setq i (1+ i))))
    (funcall tref d length-str1 length-str2)))

(provide 'init-latex)
