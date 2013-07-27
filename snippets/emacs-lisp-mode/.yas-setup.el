(defun yas/wordify (s)
  "Transform a mail adress by replacing \".\" by \" dot \" and
\"@\" by \" at \"."
  (let ((rep '(("@" . " at ")
               ("\\." . " dot "))))
    (mapc
     (lambda (e)
       (setq s (replace-regexp-in-string (car e) (cdr e) s)))
     rep)
    s))

(defun yas/insert-url-maybe ()
  (let ((backend
         (catch 'found
           (dolist (backend vc-handled-backends)
             (let ((path (vc-call-backend backend 'responsible-p default-directory)))
               (if path (throw 'found backend)))))))
    (if backend
        (concat ";; URL: "
                (cond
                 ((eq 'Git backend)
                  (with-temp-buffer
                    (vc-git-command t nil nil "config" "--get" "remote.origin.url")
                    (buffer-string)))
                 (t "")))
      "")))
