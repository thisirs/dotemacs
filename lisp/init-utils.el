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

(defun generate-password (&optional length)
  "Generate a password of length LENGTH."
  (interactive "sLength of password (10): ")
  (setq length (cond ((numberp length) length)
                     ((and (stringp length) (> (length length) 0))
                      (string-to-number length))
                     (t 10)))
  (random t)
  (let ((range "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!#$@_+,?[].-")
        (exclude "io0O1l") c result)
    (dotimes (i length result)
      (while (progn
               (setq c (aref (string-to-vector range) (random (length range))))
               (memq c (string-to-list exclude))))
      (setq result (cons c result)))
    (if (called-interactively-p 'any)
        (insert (concat result))
      (concat result))))

(defun generate-passphrase (wordlist &optional number)
  "Generate passphrase consisting of NUMBER words taken from a
file WORDLIST."
  (if (executable-find "shuf")
      (let* ((number (if (numberp number) number 4))
             (shuf-cmd (format "shuf -n %d %s" number (shell-quote-argument wordlist)))
             (pass-out (shell-command-to-string shuf-cmd)))
        (mapconcat 'identity (split-string pass-out) " "))
    (error "Unable to find program `shuf'")))

(provide 'init-utils)
