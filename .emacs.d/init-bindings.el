;; shortcut for reverting a buffer
(global-set-key (kbd "C-x C-r") 'revert-buffer)

(global-set-key (kbd "<C-kp-6>") 'enlarge-window-horizontally)
(global-set-key (kbd "<C-kp-4>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-kp-2>") 'enlarge-window)
(global-set-key (kbd "<C-kp-8>") 'shrink-window)

(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

(global-set-key (kbd "C-z") 'shell)

;; move between windows with meta-arrows
;; (windmove-default-keybindings 'meta)
(global-set-key (kbd "s-b") 'windmove-left)
(global-set-key (kbd "s-f") 'windmove-right)
(global-set-key (kbd "s-p") 'windmove-up)
(global-set-key (kbd "s-n") 'windmove-down)

;; ouverture rapide avec la touche windows
(global-set-key (kbd "s-s s") ;; scratch
                (lambda () (interactive) (switch-to-buffer "*scratch*")))
(global-set-key (kbd "s-s e") ;; .emacs
                (lambda () (interactive) (find-file (file-truename "~/.emacs.d/init.el"))))
(global-set-key (kbd "s-s m") ;; messages
                (lambda () (interactive) (switch-to-buffer "*Messages*")))
(global-set-key (kbd "s-s t") ;; twittering-mode
                (lambda () (interactive) (switch-to-buffer ":home")))


(global-set-key (kbd "C-x à") 'delete-other-windows)
(global-set-key (kbd "C-x C-à") 'delete-other-windows)
(global-set-key (kbd "C-,") 'other-window)


(global-set-key (kbd "C-c t") 'toggle-transparency)

;; automatically indent wherever I am
(global-set-key (kbd "RET") 'newline-and-indent)

(global-set-key [\C-home] 'beginning-of-buffer)
(global-set-key [\C-end] 'end-of-buffer)

;; fuck occur and word isearch
(global-set-key (kbd "M-s") 'backward-kill-word)

(global-set-key [(control tab)] 'other-window)

(provide 'init-bindings)
