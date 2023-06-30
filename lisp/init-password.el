;; -*- lexical-binding: t -*-
;; Passwords management with org files + auth-source backend

;; https://git.leafac.com/org-password-manager
(use-package org-password-manager       ; Minimal password manager for Emacs Org Mode.
  :elpaca (org-password-manager
             :type git
             :host github
             :repo "thisirs/org-password-manager")
  :defer
  :config
  (require 'auth-source)

  (setq org-password-manager-scope 'file
        org-password-manager-timeout "30")

  (defvar org-password-manager-yank-password ()
    "Store a lambda function that yield the password.")

  (defun org-password-manager-store-password (password &optional timeout)
    (setq org-password-manager-yank-password
          (let ((i 0) (password password))
            (lambda ()
              (if (> i 1)
                  (and (setq org-password-manager-yank-password 'ignore) nil)
                (setq i (1+ i))
                password))))
    (if (numberp timeout)
        (run-with-timer timeout nil (lambda () (setq org-password-manager-yank-password 'ignore)))))

  (define-key read-passwd-map (kbd "C-y") 'org-password-manager-insert-password)

  (defun org-password-manager-insert-password ()
    (interactive)
    (let ((password (funcall org-password-manager-yank-password)))
      (if password
          (insert password)
        (yank))))

  (defun org-password-manager-get-pass ()
    (let* ((property-entries
            (delq nil
                  (org-map-entries
                   (lambda ()
                     (let ((heading (org-link-display-format (org-get-heading t t))))
                       (set-text-properties 0 (length heading) nil heading)
                       (when heading
                         (list (org-link-display-format heading)
                               (or (org-entry-get (point) "LOGIN")
                                   (org-entry-get (point) "USER")
                                   (org-entry-get (point) "NAME")
                                   heading)
                               (org-entry-get (point) "PASSWORD")))))
                   (concat "PASSWORD" "={.+}") org-password-manager-scope)))
           (chosen-heading
            (let ((history-delete-duplicates t))
              (funcall 'org-completing-read
                       "Password for: "
                       property-entries
                       nil
                       nil
                       nil
                       'org-password-manager-history
                       nil)))
           (header-property-list (assoc chosen-heading property-entries))
           (header (nth 0 header-property-list))
           (login (nth 1 header-property-list))
           (password (nth 2 header-property-list)))
      (cond (password
             (org-password-manager-store-password password)
             (run-at-time org-password-manager-timeout nil
                          (lambda () (setq org-password-manager-yank-password 'ignore)))
             (funcall interprogram-cut-function password)
             (run-at-time org-password-manager-timeout nil
                          (lambda () (funcall interprogram-cut-function "")))
             (message "Password for `%s' with login `%s' copied to system's clipboard" header login))
            (t
             (message "No stored password"))))))

(use-package auth-source-org
  :elpaca `(auth-source-org
            :repo ,(expand-file-name "auth-source-org" projects-directory)))


(provide 'init-password)
