;; auto-fill
(setq comment-auto-fill-only-comments t)

;; Turns on auto-fill everywhere except in minibuffer
(defun do-auto-fill-exept-in-minibuffer ()
  (unless (minibufferp)
    (do-auto-fill)))

(setq-default auto-fill-function 'do-auto-fill-exept-in-minibuffer)

(global-set-key [C-return] #'comment-indent-new-line)

;; https://github.com/purcell/unfill
(use-package unfill                     ; Unfill paragraphs or regions, and toggle between filled & unfilled
  :straight t
  :bind ("M-q" . unfill-toggle))

(provide 'init-fill)
