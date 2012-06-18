;;; dired, dired-x and co
(add-hook 'dired-load-hook
          (function (lambda ()
                      (load "dired-x")
                      ;; Set global variables here.  For example:
                      ;; (setq dired-guess-shell-gnutar "gtar")
                      )))

;; if dired is not loaded "\C-x\C-j" is not bound to `dired-jump'
(autoload 'dired-jump "dired-x")
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
  '(progn
     (define-key dired-mode-map "s" dired-sort-map)
     ;; Reload dired after creating a directory
     (defadvice dired-create-directory (after revert-buffer-after-create activate)
       (revert-buffer))
     ;; Delete with C-x C-k to match file buffers and magit
     (define-key dired-mode-map (kbd "C-x C-k") 'dired-do-delete)))



(provide 'init-dired)
