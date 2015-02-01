;; ;;; Ruby
;; (autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process" t)
;; (autoload 'inf-ruby-setup-keybindings "inf-ruby" "" t)

;; (add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
;; (add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
;; (add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))

;; (define-key 'help-command (kbd "R") 'yari-helm)

;; (defun ruby-setup ()
;;   ;; turn off the annoying input echo in irb
;;   (setq comint-process-echoes t)

;;   (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent))

;; (add-hook 'ruby-mode-hook 'inf-ruby-setup-keybindings)
;; (add-hook 'ruby-mode-hook 'ruby-setup)

(with-eval-after-load "company"
    (push 'company-robe company-backends))
(add-hook 'ruby-mode-hook 'robe-mode)

(provide 'init-ruby)
