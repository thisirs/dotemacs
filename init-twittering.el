(setq twittering-use-master-password t)
(setq twittering-icon-mode t)

(defun twittering-enter-next-uri ()
  (interactive)
  (let ((p (point))
        (bound (point-at-eol)))
    (twittering-goto-next-uri)
    (when (and (not (eq (point) p))
               (< (point) bound))
      (twittering-enter))
    (goto-char p)))

(add-hook
 'twittering-mode-hook
 (lambda ()
   (define-key twittering-mode-map (kbd "C-c C-o") 'twittering-enter-next-uri)
   (define-key twittering-mode-map (kbd "C-c C-q") 'twittering-kill-buffer)
   (define-key twittering-mode-map (kbd "q") 'bury-buffer)))

(add-hook 'twittering-new-tweets-hook
          (lambda ()
            (let ((n twittering-new-tweets-count))
              (when (and (> n 10)
                         (require 'notifications nil t))
                (notifications-notify
                 :title
                 (twittering-timeline-spec-to-string twittering-new-tweets-spec)
                 :body
                 (format "You have %d new tweet%s" n (if (> n 1) "s" "")))))))

(provide 'init-twittering)
