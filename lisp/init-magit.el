(require 'magit)

(setq magit-last-seen-setup-instructions "1.4.0")

(global-set-key "\C-ci" 'magit-status)

(define-key magit-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((current-prefix-arg 4))
      (magit-visit-item 'other-window))))

(provide 'init-magit)
