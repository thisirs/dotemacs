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
          (when-let* ((file (buffer-local-value 'buffer-file-name buf)))
            (unless (file-remote-p file)
              (let ((location (if (eq (buffer-local-value 'major-mode buf) 'dired-mode)
                                   (buffer-local-value 'default-directory buf)
                                file)))
                (and (stringp location)
                     (string-prefix-p
                      (let (file-name-handler-alist)
                        (file-truename (abbreviate-file-name path)))
                      (file-truename (abbreviate-file-name location)))
                     (funcall fun buf))))))
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

;; https://github.com/mclear-tools/dotemacs
(defun make-parent-directory ()
  "Make sure the directory of `buffer-file-name' exists."
  (make-directory (file-name-directory buffer-file-name) t))

(add-hook 'find-file-not-found-functions #'make-parent-directory)

(provide 'init-find-file)
