;; -*- lexical-binding: t -*-
;; Credentials kept in an encrypted org file: retrieval from Emacs,
;; export to a browserpass-readable store, auth-source backend.

;; https://github.com/thisirs/org-password-manager
(use-package org-password-manager       ; Password manager for Org mode
  :ensure `(org-password-manager
            :repo ,(expand-file-name "org-password-manager" projects-directory))
  ;; Loaded on idle rather than on first use: the store has to follow
  ;; every save of the org file, not only the saves that happen after a
  ;; password was asked for.
  :defer 5
  :custom
  (org-password-manager-file "~/SynologyDrive/Sylvain/Org/personnel.org.gpg")
  (org-password-manager-scope 'file)
  ;; C-c C-p p, g and u, in the personal file only.
  :hook (org-mode . org-password-manager-maybe-enable)
  :config
  ;; C-y at a `read-passwd' prompt inserts the password just copied.
  (org-password-manager-setup-read-passwd)

  ;; Regenerate ~/.password-store on every save of the org file, so that
  ;; browserpass fills credentials straight into Firefox pages.
  (require 'org-password-manager-store)
  (org-password-manager-store-auto-export-mode 1))

;; The `auth-source' backend ships with the same package, and registers
;; itself through its autoloads.  Org files still have to be named in
;; `auth-sources' to be searched:
;; (add-to-list 'auth-sources "~/SynologyDrive/Sylvain/Org/personnel.org.gpg")

(provide 'init-password)
