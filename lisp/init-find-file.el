;; ;; Open certain files externally with ido, dired, find-file
;; (defvar find-file-externally '("ods" "odt" "pdf" "docx" "doc" "xls" "xlsx" "avi" "mp4" "flv")
;;   "List of file extensions that should be opened externally.")

;; (defun find-file-noselect-org-open (oldfun filename &optional nowarn rawfile wildcards)
;;   "Advice on `find-file-noselect' to open files externally."
;;   (if (string-match (concat "\\." (regexp-opt find-file-externally t) "$") filename)
;;       (prog1 nil
;;         (org-open-file filename)
;;         (message "Opening file externally"))
;;     (funcall oldfun filename nowarn rawfile wildcards)))

;; (defvar find-file-open-externally t
;;   "If non-nil, open specific files externally.")

;; (if find-file-open-externally
;;     (advice-add 'find-file-noselect :around #'find-file-noselect-org-open)
;;   (advice-remove 'find-file-noselect 'find-file-noselect-org-open))

(defun revert-all ()
  "Revert all buffers without asking."
  (interactive)
  (let ((bl (buffer-list)) buf)
    (while (setq buf (pop bl))
      (condition-case nil
          (with-current-buffer buf
            (let ((revert-without-query '(".")))
              (revert-buffer t)))))))

(defun dwim-location (path fun)
  "Call FUN on each buffer visiting a file contained in PATH."
  (mapc (lambda (buf)
          (let ((location (if (eq (buffer-local-value 'major-mode buf) 'dired-mode)
                              (buffer-local-value 'default-directory buf)
                            (buffer-file-name buf))))
            (and (stringp location)
                 (string-prefix-p
                  (let (file-name-handler-alist)
                    (file-truename (abbreviate-file-name path)))
                  (file-truename (abbreviate-file-name location)))
                 (funcall fun buf))))
        (buffer-list)))

(defun kill-location (path)
  "Kill all buffers visiting a file contained in PATH."
  (interactive "fLocation: ")
  (dwim-location path 'kill-buffer))

(defun revert-location (path)
  "Revert all buffers visiting a file contained in PATH."
  (interactive "fLocation: ")
  (dwim-location path (lambda (buf)
                        (with-current-buffer buf
                          (revert-buffer nil t)))))

;; Taken from http://iqbalansari.github.io/blog/2014/12/07/automatically-create-parent-directories-on-visiting-a-new-file-in-emacs/
(defun my-create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions #'my-create-non-existent-directory)

(provide 'init-find-file)
