;; auto-fill
;; (setq comment-auto-fill-only-comments t)

;; Turns on auto-fill everywhere except in minibuffer
;; (defun do-auto-fill-exept-in-minibuffer ()
;;   (unless (minibufferp)
;;     (do-auto-fill)))

;; (setq-default auto-fill-function 'do-auto-fill-exept-in-minibuffer)

(keymap-global-set "C-RET" #'comment-indent-new-line)

(provide 'init-fill)
