(use-package dired
  :ensure nil
  :preface
  (defun turn-on-truncate-lines ()
    (toggle-truncate-lines 1))

  :hook ((dired-mode-hook . turn-on-auto-revert-mode)
         (dired-mode-hook . dired-hide-details-mode)
         (dired-mode-hook . turn-on-truncate-lines))
  :init
  (defun dired-copy-filename-as-kill-fix (&optional arg)
    (interactive "P")
    (cond ((equal arg '(4))
           (dired-copy-filename-as-kill '(0)))
          ((equal arg '(16))
           (let* ((file (car (dired-get-marked-files nil t)))
                  (root (vc-responsible-backend-root file))
                  (string (file-relative-name file root)))
             (kill-new string)
             (message "%s" string)))
          (t (dired-copy-filename-as-kill arg))))

  ;; Adapted from http://stackoverflow.com/a/19112313/1299368
  (defun dired-ediff-marked-files ()
    "Run ediff-files on a pair of files marked in dired buffer"
    (interactive)
    (let* ((marked-files (dired-get-marked-files))
           (other-win (get-window-with-predicate
                       (lambda (window)
                         (with-current-buffer (window-buffer window)
                           (and (not (eq window (selected-window)))
                                (eq major-mode 'dired-mode))))))
           (other-marked-files (and other-win
                                    (with-current-buffer (window-buffer other-win)
                                      (dired-get-marked-files nil)))))
      (cond ((= (length marked-files) 2)
             (let ((invert (if (apply #'file-newer-than-file-p marked-files)
                               'nreverse
                             'identity)))
               (apply 'ediff-files (funcall invert marked-files))))
            ((and (= (length marked-files) 1)
                  (= (length other-marked-files) 1))
             (if (file-newer-than-file-p (car marked-files)
                                         (car other-marked-files))
                 (ediff-files (car other-marked-files)
                              (car marked-files))
               (ediff-files (car marked-files)
                            (car other-marked-files))))
            ((= (length marked-files) 1)
             (read-file-name "file: " (dired-dwim-target-directory)))
            (t (error "mark exactly 2 files, at least 1 locally")))))

  (defun dired-open (&optional file-list)
    (interactive
     (list (dired-get-marked-files t current-prefix-arg)))
    (apply 'call-process "xdg-open" nil 0 nil file-list))

  (defmacro dired-define-sorter (symbol switches doc)
    `(defun ,symbol ()
       ,doc
       (interactive)
       (dired-sort-other
        (concat dired-listing-switches
                (unless (string-match "-r" dired-actual-switches)
                  " -r")
                ,switches))))

  (dired-define-sorter dired-sort-by-name "" "Sort by name")
  (dired-define-sorter dired-sort-by-extension " -X" "Sort by extension")
  (dired-define-sorter dired-sort-by-size " -S" "Sort by size")
  (dired-define-sorter dired-sort-by-time " -t" "Sort by time")
  (dired-define-sorter dired-sort-directory-first " --group-directories-first" "Sort directories first")

  :bind (("C-x C-j" . dired-jump)
         :map dired-mode-map
         :prefix-map dired-sort-map
         :prefix "s"
         ("x" . dired-sort-by-extension)
         ("s" . dired-sort-by-size)
         ("t" . dired-sort-by-time)
         ("n" . dired-sort-by-name)
         ("d" . dired-sort-directory-first)
         :map dired-mode-map
         ("C-j" . dired-up-directory)
         ("<C-return>" . dired-open)
         ("e" . dired-ediff-marked-files)
         ("M-!" . async-shell-command)
         ("C-j" . dired-up-directory)
         ("C-x C-k" . dired-do-delete)
         ("w" . dired-copy-filename-as-kill-fix)
         ("@" . (lambda () (interactive) (dired "/tmp")))
         ("/" . (lambda () (interactive) (dired "/")))
         ("~" . (lambda () (interactive) (dired (getenv "HOME")))))

  :custom
  (dired-movement-style 'cycle)

  :config
  (setq dired-recursive-copies 'always)
  (setq dired-dwim-target t)

  (setq dired-clean-confirm-killing-deleted-buffers nil)

  ;; Automatically revert Dired buffer on revisiting.
  (setq dired-auto-revert-buffer t)

  (setq dired-do-revert-buffer t)

  ;; But show symlinks
  (setq dired-hide-details-hide-symlink-targets nil)

  (setq auto-revert-verbose nil))

(use-package dired-aux
  :ensure nil
  :custom
  (dired-create-destination-dirs 'ask))

(use-package dired-x
  :ensure nil
  :demand :after dired)

(defun dired-bury-all (&optional kill)
  "Bury dired buffer as they appear. It keeps you from pressing
repeatedly q."
  (interactive "P")
  (while (or (and (eq major-mode 'dired-mode)
                  (or (quit-window kill) t))
             (and (eq major-mode 'help-mode)
                  (or (quit-window kill) t))
             (and (derived-mode-p 'compilation-mode)
                  (or (quit-window kill) t)))))

;; Don't wait for auto-revert and reload dired after creating a directory
;; (advice-add 'dired-create-directory :after (lambda (&rest args) (revert-buffer)))

;; (define-key dired-mode-map (kbd "Q") #'dired-bury-all)

;; C-c C-m C-a jumps to gnus with current file attached
(use-package gnus-dired
  :ensure nil
  :custom (gnus-dired-mail-mode 'mu4e-user-agent)
  :hook (dired-mode-hook . turn-on-gnus-dired-mode))

;; (defvar dired-do-command-result nil)

;; (defun dired-do-command (command &optional reduce)
;;   "Run COMMAND on marked files. Any files not already open will be opened.
;; After this command has been run, any buffers it's modified will remain
;; open and unsaved."
;;   (interactive (list (read--expression "Command: ")
;;                      (read--expression "Reduce return values (default list): "
;;                                        "list")))
;;   (unless reduce (setq reduce 'list))
;;   (setq dired-do-command-result
;;         (apply reduce
;;                (mapcar
;;                 (lambda (filename)
;;                   (save-window-excursion
;;                     (switch-to-buffer (find-file-noselect filename))
;;                     (if (symbolp command)
;;                         (call-interactively command)
;;                       (eval command))))
;;                 (dired-get-marked-files nil nil 'dired-nondirectory-p)))))

;; (defmacro with-current-value (variable buffers &rest body)
;;   "Execute the forms in BODY with VARIABLE temporarily set to
;; current in all the buffers BUFFERS. The value returned is the
;; value of the last form in BODY."
;;   (declare (indent 2))
;;   (let ((current-variable (make-symbol "current-variable"))
;;         (saved-variables (make-symbol "saved-variables")))
;;     `(let* ((,current-variable ,variable)
;;             (,saved-variables
;;              (mapcar (lambda (buf)
;;                        (list buf (local-variable-p ',variable buf)
;;                              (buffer-local-value ',variable buf)))
;;                      ,buffers)))
;;        (mapc (lambda (buf)
;;                (with-current-buffer buf
;;                  (setq ,variable ,current-variable)))
;;              ,buffers)
;;        (prog1 (progn ,@body)
;;          ;; restore variable value
;;          (mapc (lambda (e)
;;                  (with-current-buffer (car e)
;;                    (if (cadr e)
;;                        (setq ,variable (caddr e))
;;                      (kill-local-variable ',variable))))
;;                ,saved-variables)))))

;; (defun dired-do-occur (regexp &optional nlines)
;;   "View lines which match REGEXP in all marked buffers.
;; Optional argument NLINES says how many lines of context to display: it
;; defaults to one. "
;;   (interactive (occur-read-primary-args))
;;   (if (or (not (integerp nlines))
;;           (< nlines 0))
;;       (setq nlines 0))
;;   (let ((marked-buffers (mapcar 'find-file-noselect (dired-get-marked-files))))
;;     (with-current-value case-fold-search marked-buffers
;;       (occur-1 regexp nlines marked-buffers))))

;; (use-package dired-ranger
;;   :bind (:map dired-mode-map
;;               ("W" . dired-ranger-copy)
;;               ("X" . dired-ranger-move)
;;               ("Y" . dired-ranger-paste)))

(provide 'init-dired)
