;; Taken from http://endlessparentheses.com/easily-create-github-prs-from-magit.html
(defun visit-pull-request-url ()
  "Visit the current branch's PR on Github."
  (interactive)
  (browse-url
   (format "https://github.com/%s/pull/new/%s"
           (replace-regexp-in-string
            "\\`.+github\\.com/\\(.+\\)\\.git\\'" "\\1"
            (magit-get "remote"
                       (magit-get-remote)
                       "url"))
           (cdr (magit-get-remote-branch)))))

(eval-after-load 'magit
  '(define-key magit-mode-map "V"
     #'visit-pull-request-url))

(require 'magit)

(global-set-key "\C-ci" 'magit-status)

(define-key magit-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((current-prefix-arg t))
      (call-interactively 'magit-diff-visit-file))))

(provide 'init-magit)
