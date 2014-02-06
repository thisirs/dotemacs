(defun latex-escape-or-unescape-accented-characters (&optional escape)
  "Escapes accented characters when no prefix argument. When
  escaping, the first element of a list is preferred when there
  is a list. When any prefix argument, unescape accented
  characters."
  (interactive "P")
  (let ((n 0)
        (beg (copy-marker
              (if (region-active-p) (region-beginning) (point-min))))
        (end (copy-marker
              (if (region-active-p) (region-end) (point-max)))))
    (save-excursion
      (mapc
       (lambda (e)
         (let* ((to (if escape (cdr e) (car e)))
                (to (if (listp to) (car to) to))
                (from (if escape (car e) (cdr e)))
                (from (if (listp from) from (list from))))
           (mapc
            (lambda (froma)
              (goto-char beg)
              (while (search-forward froma end t)
                (replace-match to nil t)
                (setq n (+ n 1))))
            from)))
       '((("\\'e" "\\'{e}") . "é")
         (("\\`e" "\\`{e}") . "è")
         (("\\`a" "\\`{a}") . "à")
         (("\\`u" "\\`{u}") . "ù")
         (("\\^e" "\\^{e}") . "ê")
         (("\\^o" "\\^{o}") . "ô")
         (("\\^u" "\\^{u}") . "û")
         (("\\\"i" "\\\"{i}") . "ï")
         ("\\c{c}" . "ç")
         (("\\^i" "\\^{i}") . "î"))))
    (message "Replaced %d occurences" n)))

(defun enclosing-braces-at-point ()
  (and (thing-at-point-looking-at "{\\([^}]*\\)}")
       (buffer-substring-no-properties (match-beginning 1) (match-end 1))))

(defun latex-occur-ref-wo-tilde ()
  (interactive)
  (occur "[^~(](?\\\\\\(eq\\|page\\|[fvF]\\)?ref"))

(defun latex-occur-ref-with-parent ()
  (interactive)
  (occur "(\\\\ref{[^{]*})"))

(defun latex-rename-figure-label ()
  "Refactor labels following an \\includegraphics.

The new label is the file-name used in \\includegraphics. A label
is created when it doesn't exist."
  (interactive)
  (save-excursion
    (let ((bound (condition-case nil
                     (save-excursion (LaTeX-find-matching-end) (point))
                   (error (prog1
                              (point-max)
                            (goto-char (point-min)))))))
      (while (re-search-forward
              "\\\\includegraphics\\(\\[[^]]+\\]\\)?{\\([^}]+\\)}" bound t)
        (let ((label (match-string-no-properties 2))
              (end (save-excursion
                     (re-search-forward "\\\\end{\\(sub\\)figure}" nil t)
                     (point))))
          (if (re-search-forward "\\\\label{\\([^}]+\\)" end t)
              (unless (equal label (match-string-no-properties 1))
                (save-window-excursion
                  (save-current-buffer
                    (condition-case nil
                        (reftex-query-replace-document
                         (concat "{" (regexp-quote (match-string-no-properties 1)) "}")
                         (format "{%s}" label))
                      (user-error nil)))))
            (goto-char end)
            (backward-sexp 2)
            (insert (format "\\label{%s}" label))))))))

(defun latex-replace-by-closest (path)
  "Replace filename in all includegraphics by closest filename
with respect to Levenshtein distance. Candidates are found in the
subdirectory img and filtered by extension."
  (interactive "D")
  (let ((img-files (directory-files path nil)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\\\includegraphics\\(\\[[^]]+\\]\\)?{\\([^}]+\\)" nil t)
        (let* ((old (match-string-no-properties 2))
               (closest "")
               (new
                (dolist (file img-files closest)
                  (if (> (levenshtein-distance old closest)
                         (levenshtein-distance old (file-name-sans-extension file)))
                      (setq closest (file-name-sans-extension file))))))
          (when (and
                 (> (length new-filename) 0)
                 (not (equal new-filename old-filename))
                 (yes-or-no-p
                  (format "Replace %s by %s? " old-filename new-filename))
                 (replace-match new-filename nil nil nil 2))))))))

;; Taken from http://en.wikibooks.org/wiki/Algorithm_Implementation/Strings/Levenshtein_distance#Emacs_Lisp
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

(defun latex-replace-newline ()
  "Find latex newline commands `\\\\' and replace them by a double
newline. The matrix environment are skipped."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\\\\\\\" nil t)
      (when (and (not (TeX-in-comment))
                 (not (member (LaTeX-current-environment)
                              '("pmatrix" "matrix" "array" "eqnarray" "eqnarray*"
                                "align" "align*" "aligned" "cases" "equation*"
                                "equation")))
                 (y-or-n-p "Replace \\\\? "))
        (delete-char -2)
        (TeX-newline)
        (unless (looking-at " *\n")
          (TeX-newline))))))

(defun latex-update-label-current-environment ()
  "Insert or replace labels in the enclosing figure environment.
The new label is the name of the included file."
  (interactive)
  (let ((curr-env (LaTeX-current-environment)))
    (if (not (string-match "\\(sub\\)?figure" curr-env))
        (error "Not in a figure environment")
      (save-excursion
        (save-restriction
          (LaTeX-mark-environment)
          (narrow-to-region (region-beginning) (region-end))
          (while (re-search-forward
                  "\\\\includegraphics\\(\\[[^]]+\\]\\)?{\\([^}]+\\)}" nil t)
            (let ((newname ((lambda (filename)
                              (substring filename
                                         0
                                         (string-match
                                          (concat
                                           (regexp-opt '(".jpeg" ".png" ".pdf"))
                                           "$")
                                          filename)))
                            (match-string-no-properties 2))))
              ;; skip caption for correct numbering
              (skip-chars-forward " \n\t")
              (if (looking-at "\\\\caption") (forward-sexp 2))
              (skip-chars-forward " \n\t")
              (if (looking-at "[ \t\n]*\\\\label{\\([^}]+\\)")
                  (unless (equal (match-string-no-properties 1) newname)
                    (replace-match newname nil nil nil 1))
                (open-line 1)
                (insert (format "\\label{%s}" newname))
                (indent-for-tab-command)))))))))

(defun multilang (beg end)
  (interactive "r")
  (let ((curr-text (delete-and-extract-region beg end)))
    (if (and (not (string-match "\n" curr-text))
             (< (+ (current-column) 9 (* 2 (length curr-text)))
                80))
        (insert (format "\\lang{%s}{%s}" curr-text curr-text))
        (insert (format "\
\\lang{%%
%s}{%%
%s}" curr-text curr-text))))
  (indent-region beg (point)))

(provide 'init-latex)
