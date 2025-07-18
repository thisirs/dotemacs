;; -*- lexical-binding: t; -*-
(defun yas/maybe-escape (text)
  (let ((first-char (if (> (length text) 0) (substring text 0 1))))
    (cond
     ((equal first-char "{") "\\{"))))

(defun yas/corresponding-delim (text)
  (let ((first-char (if (> (length text) 0) (substring text 0 1)))
        (second-char (if (> (length text) 1) (substring text 1 2))))
    (cond
     ((equal first-char "(") ")")
     ((equal first-char "[") "]")
     ((equal first-char "{") "}")
     ((equal first-char "\\")
      (cond
       ((equal second-char "(") ")")
       ((equal second-char "[") "]")
       ((equal second-char "{") "\\}"))))))
