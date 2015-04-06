;; Taken from http://endlessparentheses.com/easily-create-github-prs-from-magit.html
(defun visit-pull-request-url ()
  "Visit the current branch's PR on Github."
  (interactive)
  (browse-url
   (format "https://github.com/%s/compare/%s"
           (replace-regexp-in-string
            "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
            (magit-get "remote"
                       (magit-get-current-remote)
                       "url"))
           (magit-get-current-branch))))

(eval-after-load 'magit
  '(define-key magit-mode-map "V"
     #'visit-pull-request-url))

(require 'magit)

(setq magit-last-seen-setup-instructions "1.4.0")

(global-set-key "\C-ci" 'magit-status)

(define-key magit-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((current-prefix-arg 4))
      (magit-visit-item 'other-window))))

(provide 'init-magit)
