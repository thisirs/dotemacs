;; auto-fill
(setq comment-auto-fill-only-comments t)

;; Turns on auto-fill everywhere...
(setq-default auto-fill-function 'do-auto-fill)

(global-set-key [C-return] #'comment-indent-new-line)

(defun unfill-paragraph ()
  "Replace newline chars in current paragraph by single spaces."
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph)))

(global-set-key (kbd "M-Q") #'unfill-paragraph)

(provide 'init-fill)
