;; Passwords management with org files + auth-source backend

(use-package org-password-manager
  :config
  (defvar org-password-manager-yank-password ()
    "Store a lambda function that yield the password.")

  (defun org-password-manager-store-password (password &optional timeout)
    (setq org-password-manager-yank-password
          (lexical-let ((i 0) (password password))
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
            (org-map-entries
             (lambda ()
               (let ((heading (org-link-display-format (org-get-heading t t))))
                 (list heading
                       (or (org-entry-get (point) "LOGIN")
                           (org-entry-get (point) "USER")
                           (org-entry-get (point) "NAME")
                           heading)
                       (org-entry-get (point) "PASSWORD"))))
             (concat "PASSWORD" "={.+}") org-password-manager-scope))
           (chosen-heading (funcall 'org-completing-read
                                    "Password for: "
                                    property-entries
                                    nil
                                    nil
                                    nil
                                    'org-password-manager-history
                                    nil))
           (header-property-list (assoc chosen-heading property-entries))
           (header (nth 0 header-property-list))
           (login (nth 1 header-property-list))
           (password (nth 2 header-property-list)))
      (cond (password
             (org-password-manager-store-password password)
             (run-at-time org-password-manager-default-password-wait-time nil
                          (lambda () (setq org-password-manager-yank-password 'ignore)))
             (funcall interprogram-cut-function password)
             (run-at-time org-password-manager-default-password-wait-time nil
                          (lambda () (funcall interprogram-cut-function "")))
             ;; (let ((history-delete-duplicates t))
             ;;   (add-to-history 'org-password-manager-history (substring-no-properties header)))
             (message "Password for `%s' with login `%s' copied to system's clipboard" header login))
            (t
             (message "No stored password")))))

  ;; Auth-source backend
  (defun auth-source-backend-parse-advice (oldfun entry)
    "Allow gpg-encrypted Org files as secret sources."
    (if (not (and (stringp (plist-get entry :source))
                  (string-match "\\.org\\.gpg\\'" (plist-get entry :source))))
        (funcall oldfun entry)
      (auth-source-backend
       (plist-get entry :source)
       :source (plist-get entry :source)
       :type 'org
       :search-function 'auth-source-org-search
       :create-function 'auth-source-org-create)))

  (defvar auth-source-org-cache nil)

  (defun* auth-source-org-create (&rest spec
                                        &key backend
                                        secret host user port create
                                        &allow-other-keys)
    (user-error "To be written"))

  (defun* auth-source-org-search (&rest
                                  spec
                                  &key backend require create delete
                                  type max host user port
                                  &allow-other-keys)
    "Given a property list SPEC, return search matches from the :backend.
See `auth-source-search' for details on SPEC."
    ;; just in case, check that the type is correct (null or same as the backend)
    (assert (or (null type) (eq type (oref backend type)))
            t "Invalid org search: %s %s")

    (let ((results (auth-source-netrc-normalize
                    (auth-source-org-parse
                     :max max
                     :require require
                     :delete delete
                     :file (oref backend source)
                     :host (or host t)
                     :user (or user t)
                     :port (or port t))
                    (oref backend source))))

      ;; if we need to create an entry AND none were found to match
      (when (and create
                 (not results))

        ;; create based on the spec and record the value
        (setq results (or
                       ;; if the user did not want to create the entry
                       ;; in the file, it will be returned
                       (apply (slot-value backend 'create-function) spec)
                       ;; if not, we do the search again without :create
                       ;; to get the updated data.

                       ;; the result will be returned, even if the search fails
                       (apply #'auth-source-org-search
                              (plist-put spec :create nil)))))
      results))

  (defun* auth-source-org-parse (&rest
                                 spec
                                 &key file max host user port delete require
                                 &allow-other-keys)
    "Parse FILE and return a list of all entries in the file.
Note that the MAX parameter is used so we can exit the parse early."
    (if (listp file)
        ;; We got already parsed contents; just return it.
        file
      (when (file-exists-p file)
        (setq port (auth-source-ensure-strings port))
        (with-temp-buffer
          (let* ((max (or max 5000)) ; sanity check: default to stop at 5K
                 (modified 0)
                 (cached (cdr-safe (assoc file auth-source-org-cache)))
                 (cached-mtime (plist-get cached :mtime))
                 (cached-secrets (plist-get cached :secret))
                 (check (lambda (alist)
                          (and alist
                               (auth-source-search-collection
                                host
                                (or
                                 (auth-source--aget alist "machine")
                                 (auth-source--aget alist "host")
                                 t))
                               (auth-source-search-collection
                                user
                                (or
                                 (auth-source--aget alist "login")
                                 (auth-source--aget alist "account")
                                 (auth-source--aget alist "user")
                                 (auth-source--aget alist "name")
                                 t))
                               (auth-source-search-collection
                                port
                                (or
                                 (auth-source--aget alist "port")
                                 (auth-source--aget alist "protocol")
                                 t))
                               (or
                                ;; the required list of keys is nil, or
                                (null require)
                                ;; every element of require is in n(ormalized)
                                (let ((n (nth 0 (auth-source-netrc-normalize
                                                 (list alist) file))))
                                  (loop for req in require
                                        always (plist-get n req)))))))
                 result)

            (if (and (functionp cached-secrets)
                     (equal cached-mtime
                            (nth 5 (file-attributes file))))
                (progn
                  (auth-source-do-trivia
                   "auth-source-org-parse: using CACHED file data for %s"
                   file)
                  (insert (funcall cached-secrets)))
              (insert-file-contents file)
              ;; cache all netrc files (used to be just .gpg files)
              ;; Store the contents of the file heavily encrypted in memory.
              ;; (note for the irony-impaired: they are just obfuscated)
              (auth-source--aput
               auth-source-org-cache file
               (list :mtime (nth 5 (file-attributes file))
                     :secret (lexical-let ((v (mapcar #'1+ (buffer-string))))
                               (lambda () (apply #'string (mapcar #'1- v)))))))
            (goto-char (point-min))
            (let ((entries (auth-source-org-parse-entries check max))
                  alist)
              (while (setq alist (pop entries))
                (push (nreverse alist) result)))

            (when (< 0 modified)
              (when auth-source-gpg-encrypt-to
                ;; (see bug#7487) making `epa-file-encrypt-to' local to
                ;; this buffer lets epa-file skip the key selection query
                ;; (see the `local-variable-p' check in
                ;; `epa-file-write-region').
                (unless (local-variable-p 'epa-file-encrypt-to (current-buffer))
                  (make-local-variable 'epa-file-encrypt-to))
                (if (listp auth-source-gpg-encrypt-to)
                    (setq epa-file-encrypt-to auth-source-gpg-encrypt-to)))

              ;; ask AFTER we've successfully opened the file
              (when (y-or-n-p (format "Save file %s? (%d deletions)"
                                      file modified))
                (write-region (point-min) (point-max) file nil 'silent)
                (auth-source-do-debug
                 "auth-source-org-parse: modified %d lines in %s"
                 modified file)))

            (nreverse result))))))

  (defun auth-source-org-parse-entries (check max)
    "Parse up to MAX netrc entries, passed by CHECK, from the current buffer."
    (let ((adder (lambda (check alist all)
                   (when (and
                          alist
                          (> max (length all))
                          (funcall check alist))
                     (push alist all))
                   all))
          all)
      (org-mode)
      (org-map-entries
       (lambda ()
         (let (filtered-properties)
           (mapc (lambda (property)
                   (cond
                    ((member (car property) (append '("LAST_CHANGED" "CATEGORY")
                                                    org-special-properties)))
                    (t (push (cons (downcase (car property)) (cdr property))
                             filtered-properties))))
                 (org-entry-properties))
           (if filtered-properties
               (setq all (funcall adder check filtered-properties all)))))
       "PASSWORD={.+}|SECRET={.+}")
      (nreverse all)))

  (defun org-password-manager-auth-source-insinuate (&optional arg)
    (if (and arg (or (not (numberp arg)) (<= arg 0)))
        (advice-remove 'auth-source-backend-parse #'auth-source-backend-parse-advice)
      (advice-add 'auth-source-backend-parse :around #'auth-source-backend-parse-advice))))
