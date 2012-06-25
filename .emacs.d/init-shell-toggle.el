;;; shell-toggle
(autoload 'shell-toggle "shell-toggle"
  "Toggles between the shell buffer and whatever buffer you are editing."
  t)
(autoload 'shell-toggle-cd "shell-toggle"
  "Pops up a shell-buffer and insert a \"cd <file-dir>\" command." t)

(setq shell-toggle-launch-shell 'shell)

;; C-d to kill buffer if process is dead.
(defun comint-delchar-or-eof-or-kill-buffer (arg)
  (interactive "p")
  (if (null (get-buffer-process (current-buffer)))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))

(add-hook 'shell-mode-hook
          (lambda ()
            (define-key shell-mode-map (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer)))

(global-set-key (kbd "C-z") 'shell-toggle)
(global-set-key (kbd "C-M-z") 'shell-toggle-cd)

(provide 'init-shell-toggle)
