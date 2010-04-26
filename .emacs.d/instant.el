(defvar buffer-before-switch nil)
(defvar buffer-to-switch-to nil)
;;(defvar instant-modifier nil)

(defun instant-switch ()
  "Switch buffers"
  (interactive)
  (if (equal (current-buffer) buffer-to-switch-to)
    (progn
      (switch-to-buffer buffer-before-switch)
      (message (format "Switched back to %s!" buffer-before-switch)))
    (progn
      (setq buffer-before-switch (current-buffer))
      (switch-to-buffer buffer-to-switch-to)
      (message (format "Switched to %s!" buffer-to-switch-to)))))
      

(defun instant-set-buffer-to-switch-to ()
  (interactive)
  (setq buffer-to-switch-to (current-buffer))
  (message (format "Buffer %s memorized!" (current-buffer))))

(global-set-key (kbd "C-<f2>") 'instant-set-buffer-to-switch-to)

(global-set-key (kbd "<f2>") 'instant-switch)

(provide 'instant)
