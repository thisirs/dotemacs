(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/magit"))

(require 'magit)

(global-set-key "\C-ci" 'magit-status)

;; look at diff when writing a commit message
(defun magit-log-show-diff ()
  (interactive)
  (let ((content (magit-cmd-output "git" '("diff" "--cached"))))
    (set-buffer (get-buffer-create "*vc-diff*"))
    (let ((buffer-undo-list t)
          (inhibit-read-only t))
      (erase-buffer)
      (insert content))
    (diff-mode)
    (setq buffer-read-only t)
    (pop-to-buffer (current-buffer))))

(define-key magit-log-edit-mode-map (kbd "C-c C-d") 'magit-log-show-diff)

(provide 'init-magit)
