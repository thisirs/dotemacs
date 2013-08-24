;; Auto-save with non-visiting buffer is too rigid

(defun save-scratch-buffer ()
  "Create a backup of scratch buffer."
  (and (get-buffer "*scratch*")
       (with-current-buffer "*scratch*"
         (if (buffer-modified-p)
             (progn
               (set-visited-file-name
                (concat temporary-file-directory "scratch.el") t)
               (setq backup-inhibited nil)
               (save-buffer)
               (backup-buffer)
               (kill-buffer))))))

(add-hook 'kill-emacs-hook 'save-scratch-buffer)

;; Put something different in the scratch buffer
(setq initial-scratch-message
      ";; scratch buffer created -- happy hacking\n\n")

(provide 'init-scratch)
