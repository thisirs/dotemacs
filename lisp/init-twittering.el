(use-package twittering-mode
  :ensure
  :config

  (setq twittering-use-master-password t)
  (setq twittering-icon-mode t)

  ;; Cache icons
  (setq twittering-use-icon-storage t)

  ;; Fetch more tweets as twitter limits the number of requests
  (setq twittering-number-of-tweets-on-retrieval 50)

  (defun twittering-enter-next-uri ()
    (interactive)
    (let* ((p (point))
           (end (field-end p t)))
      (goto-char (field-beginning p t))
      (if (and (twittering-goto-next-uri)
               (< (point) end))
          (twittering-enter)
        (goto-char p)
        (error "No uri in this tweet"))
      (goto-char p)))

  (add-hook
   'twittering-mode-hook
   (lambda ()
     (define-key twittering-mode-map (kbd "C-c C-o") #'twittering-enter-next-uri)
     (define-key twittering-mode-map (kbd "C-c C-q") #'twittering-kill-buffer)
     (define-key twittering-mode-map (kbd "q") #'bury-buffer)))

  (add-hook 'twittering-new-tweets-hook
            (lambda ()
              (let ((n twittering-new-tweets-count))
                (when (and (> n 10)
                           (require 'notifications nil t))
                  (notifications-notify
                   :title
                   (twittering-timeline-spec-to-string twittering-new-tweets-spec)
                   :body
                   (format "You have %d new tweet%s" n (if (> n 1) "s" ""))))))))

(provide 'init-twittering)
