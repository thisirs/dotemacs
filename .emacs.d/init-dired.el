;;; dired, dired-x and co
(add-hook 'dired-load-hook
          (function (lambda ()
                      (load "dired-x")
                      ;; Set global variables here.  For example:
                      ;; (setq dired-guess-shell-gnutar "gtar")
                      )))

;; (require 'dired+)

(defvar dired-sort-map (make-sparse-keymap))
(define-key dired-mode-map "s" dired-sort-map)

(mapc
 (lambda (elt)
   (define-key dired-sort-map (car elt)
     `(lambda ()
        (interactive)
        (dired-sort-other
         (concat dired-listing-switches
                 (unless (string-match "-r" dired-actual-switches)
                   " -r") ,(cadr elt))))))
 '(("n" "")
   ("x" " -X")
   ("s" " -S")
   ("t" " -t")
   ("d" " --group-directories-first")))



(provide 'init-dired)
