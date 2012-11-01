(setq twittering-use-master-password t)

(defun twittering-enter-next-uri ()
  (interactive)
  (let ((p (point))
        (bound (point-at-eol)))
    (twittering-goto-next-uri)
    (when (and (not (eq (point) p))
               (< (point) bound))
      (twittering-enter))
    (goto-char p)))

(define-key twittering-mode-map (kbd "C-c C-o") 'twittering-enter-next-uri)

;; (add-hook 'twittering-mode-hook
;;           (lambda ()
;;             (twittering-icon-mode 1)))

(provide 'init-twittering)
