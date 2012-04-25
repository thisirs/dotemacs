;;; dired, dired-x and co
(add-hook 'dired-load-hook
          (function (lambda ()
                      (load "dired-x")
                      ;; Set global variables here.  For example:
                      ;; (setq dired-guess-shell-gnutar "gtar")
                      )))

;; if dired is not loaded "\C-x\C-j" is not bound to `dired-jump'
(define-key global-map "\C-x\C-j" 'dired-jump)

;; (require 'dired+)

(defvar dired-sort-map (make-sparse-keymap))

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

(eval-after-load 'dired
    '(define-key dired-mode-map "s" dired-sort-map))


(provide 'init-dired)
