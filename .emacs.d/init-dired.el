(require 'dired-x)

;; Make dired less verbose
(require 'dired-details)

(setq dired-details-hide-link-targets nil)

(dired-details-install)

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

(define-key dired-mode-map "s" dired-sort-map)

;; Reload dired after creating a directory
(defadvice dired-create-directory (after revert-buffer-after-create activate)
  (revert-buffer))

;; Delete with C-x C-k to match file buffers and magit
(define-key dired-mode-map (kbd "C-x C-k") 'dired-do-delete)

(defun dired-do-command (command)
  "Run COMMAND on marked files. Any files not already open will be opened.
After this command has been run, any buffers it's modified will remain
open and unsaved."
  (interactive "CRun on marked files M-x ")
  (dolist (filename (dired-get-marked-files))
    (with-current-buffer (find-file-noselect filename)
      (call-interactively command))))

(provide 'init-dired)
