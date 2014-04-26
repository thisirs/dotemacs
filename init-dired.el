(require 'dired-x)

;; Auto-revert mode
(add-hook 'dired-mode-hook 'turn-on-auto-revert-mode)
(setq auto-revert-verbose nil)

;; Auto-revert dired buffer on revisiting
(setq dired-auto-revert-buffer t)

;; Make dired less verbose
(with-emacs-version> "24.3"
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  (setq dired-hide-details-hide-symlink-targets nil))

(defun dired-bury-all (&optional kill)
  "Bury dired buffer as they appear. It keeps you from pressing
repeatedly q."
  (interactive "P")
  (while (eq major-mode 'dired-mode)
    (quit-window kill)))

;; Reload dired after creating a directory
(defadvice dired-create-directory (after revert-buffer-after-create activate)
  (revert-buffer))

(defvar dired-sort-map
  (let ((map (make-sparse-keymap)))
    (mapc
     (lambda (elt)
       (define-key map (car elt)
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
    map))

(define-key dired-mode-map (kbd "Q") 'dired-bury-all)

(define-key dired-mode-map "s" dired-sort-map)

;; Delete with C-x C-k to match file buffers and magit
(define-key dired-mode-map (kbd "C-x C-k") 'dired-do-delete)

;; Quick jump to root, user dir or /tmp
(define-key dired-mode-map (kbd "@")
  (lambda ()
    (interactive)
    (dired "/tmp")))

(define-key dired-mode-map (kbd "/")
  (lambda ()
    (interactive)
    (dired "/")))

(define-key dired-mode-map (kbd "~")
  (lambda ()
    (interactive)
    (dired (getenv "HOME"))))

;; Copy full file-path in kill-ring
(defun dired-copy-path-as-kill (&optional arg)
  "Copy full path of marked or current file. If ARG is non-nil,
ask for a base directory and return path relative to this
directory."
  (interactive "P")
  (if arg
      (let ((default-directory default-directory))
        (if (called-interactively-p 'interactive)
            (setq default-directory (read-file-name "Base directory: ")))
        (dired-copy-filename-as-kill '(4)))
    (dired-copy-filename-as-kill 0)))

(define-key dired-mode-map (kbd "W") 'dired-copy-path-as-kill)

;; C-c C-m C-a jumps to gnus with current file attached
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

(eval-after-load "dired-aux"
  '(add-to-list 'dired-compress-file-suffixes
                '("\\.zip\\'" ".zip" "unzip")))

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

(defmacro with-current-value (variable buffers &rest body)
  "Execute the forms in BODY with VARIABLE temporarily set to
current in all the buffers BUFFERS. The value returned is the
value of the last form in BODY."
  (declare (indent 2))
  (let ((current-variable (make-symbol "current-variable"))
        (saved-variables (make-symbol "saved-variables")))
    `(let* ((,current-variable ,variable)
            (,saved-variables
             (mapcar (lambda (buf)
                       (list buf (local-variable-p ',variable buf)
                             (buffer-local-value ',variable buf)))
                     ,buffers)))
       (mapc (lambda (buf)
               (with-current-buffer buf
                 (setq ,variable ,current-variable)))
             ,buffers)
       (prog1 (progn,@body)
         ;; restore variable value
         (mapc (lambda (e)
                 (with-current-buffer (car e)
                   (if (cadr e)
                       (setq ,variable (caddr e))
                     (kill-local-variable ',variable))))
               ,saved-variables)))))

(defun dired-do-occur (regexp &optional nlines)
  "View lines which match REGEXP in all marked buffers.
Optional argument NLINES says how many lines of context to display: it
defaults to one. "
  (interactive (occur-read-primary-args))
  (if (or (not (integerp nlines))
          (< nlines 0))
      (setq nlines 0))
  (let ((marked-buffers (mapcar 'find-file-noselect (dired-get-marked-files))))
    (with-current-value case-fold-search marked-buffers
      (occur-1 regexp nlines marked-buffers))))

(provide 'init-dired)
