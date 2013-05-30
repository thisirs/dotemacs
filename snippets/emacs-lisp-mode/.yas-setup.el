(defun yas/wordify (s)
  (let ((rep '(("@" . " at ")
               ("\\." . " dot "))))
    (mapc
     (lambda (e)
       (setq s (replace-regexp-in-string (car e) (cdr e) s)))
     rep)
    s))
