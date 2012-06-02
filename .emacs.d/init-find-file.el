;; ouvre un buffer en sudo via tramp
(defun th-find-file-sudo (file)
  "Opens FILE with root privileges."
  (interactive "F")
  (set-buffer
   (find-file
    (concat "/sudo::"
            (expand-file-name file)))))

(defadvice find-file (around th-find-file activate)
  "Open FILENAME using tramp's sudo method if it's read-only."
  (if (and (file-exists-p (ad-get-arg 0))
           (not (file-writable-p (ad-get-arg 0)))
           (not (file-remote-p (ad-get-arg 0)))
           (not (file-directory-p (ad-get-arg 0)))
           (y-or-n-p (concat "File "
                             (ad-get-arg 0)
                             " is read-only.  Open it as root? ")))
      (th-find-file-sudo (ad-get-arg 0))
    ad-do-it))

(defadvice find-file (around find-file-other-window activate)
  (if current-prefix-arg
      (find-file-other-window (ad-get-arg 0))
    ad-do-it))

(defun find-temp-file (extension)
  "quick find file in /tmp"
  (interactive "sExtension: ")
  (cond
   ((equal (length extension) 0) (find-file (make-temp-file "foo")))
   ((memq ?. (string-to-list extension))
    (find-file (concat "/tmp/" extension)))
   (t (find-file (concat (make-temp-file "foo") "." extension)))))

(global-set-key (kbd "C-x C-t") 'find-temp-file)

;; looseley based on yes-or-no-p
(defadvice find-file (around find-or-launch-file activate)
  "Try to open file externally if not recognized by emacs."
  (if (assoc-default (ad-get-arg 0) auto-mode-alist 'string-match)
      ad-do-it
    (let ((cursor-in-echo-area t) 
          (key 'recenter)
          (prompt "Open in emacs or via org? "))
      (while (let ((cursor-in-echo-area t))
               (when minibuffer-auto-raise
                 (raise-frame (window-frame (minibuffer-window))))
               (setq key (read-key
                          (propertize
                           (if (eq key 'recenter)
                               prompt
                             (concat "Please answer e or o.  " prompt))
                                      'face 'minibuffer-prompt)))
               (not (memq key '(101 111 113 7))))
        (ding)
        (discard-input))
      (cond
       ((eq key 101) ad-do-it)
       ((eq key 111) (org-open-file (ad-get-arg 0)))))))

(provide 'init-find-file)
