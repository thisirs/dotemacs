(defun load-file-to-list (file)
  "Return a list of forms read in file FILE."
  (if (and (file-exists-p file)
           (file-readable-p file))
      (with-temp-buffer
        (insert-file-contents file)
        (let ((marker (copy-marker 0))
              form-list form)
          (while (ignore-errors (setq form (read marker)))
            (setq form-list (cons form form-list)))
          (nreverse form-list)))))

(defmacro sexp-or-progn (&optional first &rest body)
  "Surround with `progn' if more than one sexp."
  (if body `(progn ,first ,@body) first))

(defmacro with-emacs-version>= (version &rest body)
  "Expand to BODY if current emacs version is newer or equal to
VERSION."
  (declare (indent 1) (debug t))
  (when (version<= version emacs-version)
    `(sexp-or-progn ,@body)))

(defmacro with-emacs-version> (version &rest body)
  "Expand to BODY if current emacs version is newer than VERSION."
  (declare (indent 1) (debug t))
  (when (version< version emacs-version)
    `(sexp-or-progn ,@body)))

(defmacro with-emacs-version<= (version &rest body)
  "Expand to BODY if current emacs version is older or equal to
VERSION."
  (declare (indent 1) (debug t))
  (when (version<= emacs-version version)
    `(sexp-or-progn ,@body)))

(defmacro with-emacs-version< (version &rest body)
  "Expand to BODY if current emacs version is older than VERSION."
  (declare (indent 1) (debug t))
  (when (version< emacs-version version)
    `(sexp-or-progn ,@body)))

(defmacro define-on-macro (sys &optional name)
  "Macro that defines a machine-specific macro.

SYS is the system name and NAME is used to define a `on-<NAME>'
macro. If not provided, it defaults to SYS. The `on-<NAME>' macro
can be used to execute enclosing code on specific machine. It can
also serve as a predicate telling if we are on specific machine."
  `(defmacro ,(intern (concat "on-" (or name sys))) (&rest body)
     (if body
         (when (string-prefix-p ,sys (system-name))
           `(sexp-or-progn ,@body))
       (string-prefix-p ,sys (system-name)))))

;; .dir-locals.el helper
(defun dir-locals-get-directory (file)
  "Return the directory containing the directory local file that
is read by the file FILE.
Useful if you want to set parameters depending on the location of
that directory local file."
  (let ((dir (dir-locals-find-file file)))
    (if (listp dir)
        (car dir)
      (file-name-directory dir))))

(defmacro debug-print (obj)
  "Debug macro that prints the object OBJ together with a
timestamp in a buffer named \"*debug*\". Scroll to bottom in case
the debug buffer is visible. Return OBJ so the macro can be put
inline."
  (let ((obsym (make-symbol "object")))
    `(let ((,obsym ,obj))
       (unless (get-buffer "*debug*")
         (with-current-buffer (get-buffer-create "*debug*")
           (emacs-lisp-mode)))
       (with-current-buffer  (get-buffer "*debug*")
         (goto-char (point-max))
         (or (bolp) (newline))
         (insert (format ";; [%s]\n%s" (current-time-string)
                         (pp-to-string ,obsym)))
         (if (get-buffer-window "*debug*")
             (set-window-point (get-buffer-window "*debug*") (point))))
       ,obsym)))

;;; Useful for renumbering sequence of identifiers like foo0, foo0bis,
;;; foo1000, foo1001 into foo0, foo1, foo2, foo3
(defun refactor-seq (regex replace)
  "Replace substrings match by REGEX with a REPLACE function.

REGEX is first matched sequentially and then all matches are
replaced by the result of REPLACE called with the matched string
and the index of the match."
  (interactive "sRegex: \nsReplace: ")
  (when (stringp replace)
    (unless (string-match "%\\([0-9]+\\)?d" replace)
      (user-error "Replacement string must have a %%d"))
    (setq replace (lexical-let ((replace replace))
                    (lambda (s i) (format replace i)))))
  (save-excursion
    (let (varnames)
      (goto-char (point-min))
      (while (re-search-forward regex nil t)
        (unless (member (match-string 0) varnames)
          (setq varnames (cons (match-string-no-properties 0) varnames))))
      (setq varnames (nreverse varnames))
      (goto-char (point-min))
      (while (re-search-forward regex nil t)
        (let ((i (cl-position (match-string-no-properties 0) varnames :test 'equal)))
          (replace-match (funcall replace (match-string 0) i)))))))

(defun straight-get-url (package)
  (let ((recipe (straight-recipes-retrieve package straight-recipe-repositories)))
    (straight-vc-git--destructure (straight--convert-recipe recipe)
        (repo host protocol)
      (straight-vc-git--encode-url repo host protocol))))

(defun use-package-add-url ()
  "Add url of package before use-package"
  (interactive)
  (save-excursion
    ;; (goto-char (point-min))
    (while (re-search-forward "([u]se-package[[:space:]]" nil t)
      (unless (nth 4 (syntax-ppss))
        (save-excursion
          (let* ((standard-input (current-buffer))
                 (def (save-excursion
                        (goto-char (match-beginning 0))
                        (read standard-input)))
                 (pkg-name (let ((it (plist-get (use-package-normalize-plist (cadr def) (cddr def) nil #'use-package-merge-keys)
                                                :ensure)))
                             (if (or (eq it t) (null it))
                                 (cadr def)
                               it))))
            (message "%S" pkg-name)
            (when-let* ((menu-item (elpaca-menu-item pkg-name))
                        (recipe (plist-get menu-item :recipe))
                        (url (with-demoted-errors "Error: %S" (elpaca--repo-uri recipe))))
              (unless (save-excursion
                        (forward-line -1)
                        (looking-at "[[:space:]]*;+[[:space:]]*http"))
                (save-excursion
                  (beginning-of-line)
                  (open-line 1)
                  (indent-according-to-mode)
                  (insert (format ";; %s" url))))
              (when-let* ((desc (plist-get menu-item :description)))
                (unless (or (equal desc package--default-summary)
                            (save-excursion (re-search-forward ";" (line-end-position) t)))
                  (end-of-line)
                  (indent-to comment-column 1)
                  (insert (format "; %s" desc)))))))))))

(defun copy-downloaded-file (file &optional directory)
  (interactive
   (list
    (completing-read
     "File to attach: "
     (let ((output (string-trim (shell-command-to-string "find ~/deathrow /tmp/mozilla_sylvain0 -maxdepth 1 -type f -exec ls -1t \"{}\" +;"))))
       (unless (string-empty-p output)
         (split-string output "\n"))))
    (read-directory-name "Copy in directory? " default-directory)))
  (copy-file file (expand-file-name (file-name-nondirectory file) (or directory default-directory))))

(defun UTC-autumn-from-time (time)
  "Return the autumn semester corresponding to TIME."
  (let* ((dtime (decode-time time))
         (month (nth 4 dtime))
         (year (nth 5 dtime)))
    (if (and (< month 8))
        (format "A%d" (1- year))
      (format "A%d" year))))

(defun UTC-spring-from-time (time)
  "Return the spring semester corresponding to TIME."
  (let* ((dtime (decode-time time))
         (month (nth 4 dtime))
         (year (nth 5 dtime)))
    (if (and (>= month 2))
        (format "P%d" year)
      (format "P%d" (1- year)))))

(defun UTC-semester-from-time (time)
  "Return the semester corresponding to TIME."
  (let* ((dtime (decode-time time))
         (month (nth 4 dtime))
         (year (nth 5 dtime)))
    (if (and (< month 8) (>= month 2))
        (format "P%d" year)
      (if (<= month 2)
          (format "A%d" (1- year))
        (format "A%d" year)))))

(defun projectile-fake-projects (dir)
  (car (member dir (projectile-default-projects))))

(defun projectile-default-projects ()
  (seq-filter #'file-exists-p
              (mapcar (lambda (args)
                        (apply #'format (expand-file-name "enseignements/%s/%s/%s" personal-directory) args))
                      (let ((semesters (delete-dups
                                        (list (UTC-semester-from-time (current-time))
                                              (UTC-semester-from-time
                                               (time-add
                                                (current-time)
                                                (seconds-to-time (* 60 60 24 31 2)))))))
                            (uvs '("SY02" "AOS1" "AOS2" "SY09" "SY19"))
                            (dirs '("Cours" "TP" "TD" "poly" "")))
                        (-table-flat 'list semesters uvs dirs)))))

(defun projectile-ignored-semester (truename)
  "Ignore past semesters."
  (if (string-match "\\([AP]\\)\\([0-9]\\{4\\}\\)" truename)
      (let ((year (string-to-number (match-string 2 truename)))
            (season (match-string 1 truename)))
        (let ((current-semester (UTC-semester-from-time (current-time))))
          (if (string-match "\\([AP]\\)\\([0-9]\\{4\\}\\)" current-semester)
              (let ((cyear (string-to-number (match-string 2 current-semester)))
                    (cseason (match-string 1 current-semester)))
                (or (< year cyear)
                    (and (= year cyear)
                         (string-equal cseason "A")
                         (string-equal season "P")))))))))

(defun check-filepath (&rest filepaths)
  (if-let* ((available (seq-filter #'file-exists-p filepaths)))
      (car available)
    (display-warning 'init (format "File(s) %s do not exist" (mapconcat 'identity filepaths " ")))))

(defmacro autoload-config (command library &optional docstring)
  "Define COMMAND to autoload LIBRARY.

Similar to an autoload but does not require the library to
directly define COMMAND. COMMAND is allowed to be defined after
loading LIBRARY in an `eval-after-load'."
  `(progn
     (defun ,command ()
       ,(or docstring (format "Configuration autoload for command `%s'" command))
       (interactive)
       (fmakunbound ',command)
       (require ',library)
       (unless (fboundp ',command)
           (error ,(format "Loading library `%s' did not define `%s'" library command)))
       (call-interactively ',command))))

(defun seq-insert-at (sequence elt i)
  (nconc
   (seq-subseq sequence 0 i)
   (cons elt (seq-subseq sequence i))))

(provide 'init-utils)
