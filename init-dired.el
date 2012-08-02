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

;; Quick jump to root or user dir
(define-key dired-mode-map (kbd "/")
  (lambda ()
    (interactive)
    (dired "/")))

(define-key dired-mode-map (kbd "~")
  (lambda ()
    (interactive)
    (dired (getenv "HOME"))))

(defun dired-do-command (command)
  "Run COMMAND on marked files. Any files not already open will be opened.
After this command has been run, any buffers it's modified will remain
open and unsaved."
  (interactive
   (list
    (let ((print-level nil)
          (minibuffer-history-position 0)
          (minibuffer-history-sexp-flag (1+ (minibuffer-depth))))
      (unwind-protect
          (read-from-minibuffer
           "Command: " (prin1-to-string (nth 0 command-history))
           read-expression-map t
           (cons 'command-history 0))

        ;; If command was added to command-history as a
        ;; string, get rid of that.  We want only
        ;; evaluable expressions there.
        (if (stringp (car command-history))
            (setq command-history (cdr command-history)))))))
  (dolist (filename (dired-get-marked-files))
    (with-current-buffer (find-file-noselect filename)
      (if (symbolp command)
          (call-interactively command)
        (eval command)))))

(defun dired-do-occur (regexp &optional nlines)
  "View lines which match REGEXP in all marked buffers.
Optional argument NLINES says how many lines of context to display: it
defaults to one."
  (interactive (occur-read-primary-args))
  (if (or (not (integerp nlines))
          (< nlines 0))
      (setq nlines 0))
  (occur-1 regexp nlines
           (mapcar 'find-file-noselect (dired-get-marked-files))))

(provide 'init-dired)
