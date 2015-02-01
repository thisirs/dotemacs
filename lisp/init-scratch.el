;; Auto-save with non-visiting buffer is too rigid

(defun save-scratch-buffer ()
  "Create a backup of scratch buffer."
  (and (get-buffer "*scratch*")
       (with-current-buffer "*scratch*"
         (when (buffer-modified-p)
           (set-visited-file-name (expand-file-name "scratch.el" temporary-file-directory) t)
           (setq backup-inhibited nil)
           (save-buffer)
           (backup-buffer)
           (kill-buffer)))))

(add-hook 'kill-emacs-hook 'save-scratch-buffer)

;; Put something different in the scratch buffer
(setq initial-scratch-message
      ";; scratch buffer created -- happy hacking\n\n")

(when (require-maybe 'scratch-message)

  (defun scratch-message-random ()
    (pcase (random 3)
      (0 (scratch-message-contrepet))
      (1 (scratch-message-SCMB))
      (2 (scratch-message-fortune))))

  (defun scratch-message-fortune ()
    (require 'fortune)
    (fortune-in-buffer t "~/.conky/english-idioms")
    (scratch-message-insert
     (with-current-buffer fortune-buffer-name
       (buffer-string))))

  (defun scratch-message-contrepet ()
    (require 'fortune)
    (with-demoted-errors "Error: %S"
      (fortune-in-buffer t "~/.conky/contrepétries")
      (scratch-message-insert
       (with-current-buffer fortune-buffer-name
         (buffer-string)))))

  (defun scratch-message-SCMB ()
    (if (executable-find "ruby")
        (let* ((message-buffer-name "*SCMB*")
               (message-buffer (or (get-buffer message-buffer-name)
                                   (generate-new-buffer message-buffer-name)))
               (proc (start-process "SCMB" message-buffer-name "ruby"
                                    "/home/sylvain/CloudStation/Sylvain/scripts/SCMB.rb")))
          (with-current-buffer message-buffer-name
            (let ((inhibit-read-only t))
              (erase-buffer)))
          (set-process-sentinel
           proc
           (lambda (process event)
             (when (string= "finished\n" event)
               (scratch-message-insert
                (with-current-buffer "*SCMB*"
                  (with-fill-line-by-line
                   (fill-region (point-min) (point-max)))
                  (buffer-string)))))))
      (error "Ruby is not installed")))

  (defmacro with-fill-line-by-line (&rest body)
    "Executes BODY with line by line filling settings."
    `(let ((paragraph-start "^")
           (paragraph-separate "\n")
           (fill-prefix ""))
       (progn ,@body)))

  (setq scratch-message-function 'scratch-message-random)

  (scratch-message-toggle-activate 1))

(provide 'init-scratch)
