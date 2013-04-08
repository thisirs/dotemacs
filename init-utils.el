(defun add-subdirs-to-load-path (directory)
  (mapc (lambda (subdir)
          (if (file-directory-p subdir)
              (add-to-list 'load-path (file-name-as-directory subdir))))
        (directory-files directory t "[^\\.]\\|\\(\\.\\{3,\\}\\)")))

(defun load-file-to-list (file)
  "Return a list of forms read in file FILE."
  (if (and (file-exists-p file)
           (file-readable-p file))
      (with-temp-buffer
        (insert-file-contents file)
        (let ((marker (copy-marker 0))
              form-list form)
          (while (with-demoted-errors (setq form (read marker)))
            (setq form-list (cons form form-list)))
          (nreverse form-list)))))

(defun require-maybe (feat)
  "Like `require' but display a message instead of signaling an
error."
  (or (require feat nil t)
      (not (message "Feature `%s' not loaded!" feat))))

(defmacro with-after-load (s &rest body)
  "Like `eval-after-load' but no progn or quote."
  (declare (debug t) (indent 1))
  `(eval-after-load ,s
     '(progn ,@body)))

(defmacro with-emacs-version>= (version &rest body)
  "Expand to BODY if current emacs version is newer or equal to
VERSION."
  (declare (indent 1) (debug t))
  (when ( version<= version emacs-version)
    `(progn ,@body)))

(defmacro with-emacs-version> (version &rest body)
  "Expand to BODY if current emacs version is newer than VERSION."
  (declare (indent 1) (debug t))
  (when ( version< version emacs-version)
    `(progn ,@body)))

(defmacro with-emacs-version<= (version &rest body)
  "Expand to BODY if current emacs version is older or equal to
VERSION."
  (declare (indent 1) (debug t))
  (when ( version<= emacs-version version)
    `(progn ,@body)))

(defmacro with-emacs-version< (version &rest body)
  "Expand to BODY if current emacs version is older than VERSION."
  (declare (indent 1) (debug t))
  (when ( version< emacs-version version)
    `(progn ,@body)))

(defmacro define-on-macro (sys &optional name)
  "Macro that defines a machine-specific macro."
  `(defmacro ,(intern (concat "on-" (or name sys))) (&rest body)
     (when (string-prefix-p ,sys system-name)
       `(progn ,@body))))

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

(defun patch (func pattern patch)
  "Patch a function definition by replacing `pattern' by `patch'."
  (and (byte-code-function-p (symbol-function func))
       (error "Symbol `%s' is bound to a byte compiled function." func))
  (fset func (repl (symbol-function func) pattern patch)))

(defun repl (exp pattern patch)
  (cond
   ((null exp) nil)
   ((listp exp)
    (let ((expr (repl (car exp) pattern patch)))
      (cons
       (if (equal expr pattern) patch expr)
       (repl (cdr exp) pattern patch))))
   (t exp)))

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

(provide 'init-utils)
