;; -*- lexical-binding: t -*-
;; Passwords management with org files + auth-source backend

;; https://git.leafac.com/org-password-manager
(use-package org-password-manager       ; Minimal password manager for Emacs Org Mode.
  :ensure (org-password-manager
             :type git
             :host github
             :repo "thisirs/org-password-manager")
  :preface
  (autoload-after org-password-manager-get-pass org-password-manager)
  (autoload-after org-password-manager-popup org-password-manager)
  :defer
  :config
  (require 'auth-source)

  (setq org-password-manager-scope 'file
        org-password-manager-timeout "30")

  (defvar org-password-manager-file
    "~/SynologyDrive/Sylvain/Org/personnel.org.gpg"
    "Org file scanned by `org-password-manager-popup'.")

  (defvar org-password-manager-ydotool-socket "/run/ydotoold/socket"
    "Socket of the running `ydotoold' daemon, used to type credentials.")

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


;;; Collecting and selecting entries

  (defun org-password-manager--entries ()
    "Return (HEADING LOGIN PASSWORD) for every entry of the current buffer."
    (delq nil
          (org-map-entries
           (lambda ()
             (let ((heading (org-link-display-format (org-get-heading t t))))
               (set-text-properties 0 (length heading) nil heading)
               (unless (string-empty-p heading)
                 (list heading
                       (or (org-entry-get (point) "LOGIN")
                           (org-entry-get (point) "USER")
                           (org-entry-get (point) "NAME")
                           heading)
                       (org-entry-get (point) "PASSWORD")))))
           (concat "PASSWORD" "={.+}") org-password-manager-scope)))

  (defvar org-password-manager--action nil
    "Action requested by the last `org-password-manager--read'.")

  (defun org-password-manager--exit-with (action)
    "Leave the minibuffer, asking for ACTION on the selected candidate."
    (setq org-password-manager--action action)
    (if (fboundp 'vertico-exit)
        (vertico-exit)
      (minibuffer-force-complete-and-exit)))

  (defvar org-password-manager-read-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-l")
                  (lambda () (interactive) (org-password-manager--exit-with 'login)))
      (define-key map (kbd "C-t")
                  (lambda () (interactive) (org-password-manager--exit-with 'type)))
      map)
    "Extra bindings offered while picking an entry.
`RET' copies the password, `C-l' copies the login and `C-t' types
login TAB password into the focused window.")

  (defun org-password-manager--read (entries)
    "Prompt for one of ENTRIES.  Return (ENTRY . ACTION)."
    (setq org-password-manager--action 'password)
    (let* ((history-delete-duplicates t)
           (chosen (minibuffer-with-setup-hook
                       (lambda ()
                         (use-local-map (make-composed-keymap
                                         org-password-manager-read-map
                                         (current-local-map))))
                     (completing-read "Password for: " entries nil t nil
                                      'org-password-manager-history))))
      (cons (assoc chosen entries) org-password-manager--action)))


;;; Acting on the selected entry

  (defun org-password-manager--copy (secret what heading)
    "Put SECRET on the clipboard, clearing it after the timeout.
WHAT and HEADING are only used for the echo area message."
    (funcall interprogram-cut-function secret)
    (run-at-time org-password-manager-timeout nil
                 (lambda ()
                   ;; Only clear if the secret is still there, so that
                   ;; anything copied meanwhile survives.
                   (when (equal (ignore-errors (gui-get-selection 'CLIPBOARD)) secret)
                     (funcall interprogram-cut-function ""))))
    (message "%s for `%s' copied to the clipboard, cleared in %ss"
             what heading org-password-manager-timeout))

  (defun org-password-manager--ydotool (&rest args)
    (let ((process-environment
           (cons (concat "YDOTOOL_SOCKET=" org-password-manager-ydotool-socket)
                 process-environment)))
      (apply #'call-process "ydotool" nil nil nil args)))

  (defun org-password-manager--ydotool-type (string)
    "Type STRING through ydotool, feeding it on stdin rather than argv."
    (let ((process-environment
           (cons (concat "YDOTOOL_SOCKET=" org-password-manager-ydotool-socket)
                 process-environment)))
      (with-temp-buffer
        (insert string)
        (call-process-region (point-min) (point-max) "ydotool"
                             nil nil nil "type" "-e" "0" "-f" "-"))))

  (defun org-password-manager--type-credentials (login password)
    (org-password-manager--ydotool-type login)
    (org-password-manager--ydotool "key" "15:1" "15:0") ; TAB
    (org-password-manager--ydotool-type password))

  (defun org-password-manager--dispatch (entry action &optional delay)
    "Run ACTION on ENTRY, waiting DELAY seconds first when typing."
    (pcase-let ((`(,heading ,login ,password) entry))
      (pcase action
        ('login
         (org-password-manager--copy login "Login" heading))
        ('type
         (if password
             (run-at-time (or delay 0) nil
                          #'org-password-manager--type-credentials login password)
           (message "No stored password")))
        (_
         (if password
             (progn
               (org-password-manager-store-password password)
               (run-at-time org-password-manager-timeout nil
                            (lambda () (setq org-password-manager-yank-password 'ignore)))
               (org-password-manager--copy password "Password" heading))
           (message "No stored password"))))))

  (defun org-password-manager-get-pass ()
    "Pick an entry of the current buffer and copy its password."
    (interactive)
    (let ((choice (org-password-manager--read (org-password-manager--entries))))
      (when (car choice)
        (org-password-manager--dispatch (car choice) (cdr choice)))))


;;; Standalone picker, meant to be called from a global shortcut

  (defvar org-password-manager-frame-parameters
    '((name . "org-password-manager")
      (title . "Passwords")
      (minibuffer . only)
      (width . 90)
      (height . 14)
      (undecorated . t)
      (auto-raise . t)
      (z-group . above))
    "Parameters of the frame popped up by `org-password-manager-popup'.")

  (defun org-password-manager-popup ()
    "Pick an entry of `org-password-manager-file' in a throwaway frame.
Meant to be called from outside Emacs, typically as
\\='emacsclient -e \"(org-password-manager-popup)\"\\=' bound to a
desktop-wide shortcut."
    (interactive)
    (let* ((entries (with-current-buffer (find-file-noselect org-password-manager-file)
                      (org-password-manager--entries)))
           (frame (make-frame org-password-manager-frame-parameters))
           (choice nil))
      (unwind-protect
          (with-selected-frame frame
            (select-frame-set-input-focus frame)
            (let ((vertico-count 12))
              (setq choice (org-password-manager--read entries))))
        (when (frame-live-p frame)
          (delete-frame frame)))
      (when (car choice)
        ;; The frame is gone, but the compositor needs a moment to hand
        ;; the focus back to the window we were called from.
        (org-password-manager--dispatch (car choice) (cdr choice) 0.4))))


;;; Exporting to a browserpass-readable store
;;
;; The org file stays the single source of truth; `~/.password-store' is a
;; derived, disposable artefact regenerated on save, so that browserpass can
;; fill credentials straight into Firefox pages.

  (require 'epg)
  (require 'url-parse)

  (defvar org-password-manager-store-directory "~/.password-store"
    "Directory holding the store generated from `org-password-manager-file'.")

  (defvar org-password-manager-store-exclude '("Obsolete")
    "Top-level headings whose entries are kept out of the generated store.")

  (defvar org-password-manager-store-require-url t
    "Whether an entry needs an URL property to reach the generated store.
The store exists for the browser, so entries with no URL -- ssh, smtp,
alarm codes -- have nothing to do there and are better left out of
browserpass' reach entirely.")

  (defvar org-password-manager-store-marker ".org-generated"
    "File marking a store as ours, hence safe to regenerate.")

  (defvar org-password-manager-store-auto t
    "Whether saving `org-password-manager-file' regenerates the store.")

  (defvar org-password-manager--store-hashes (make-hash-table :test 'equal)
    "Map a store-relative path to a hash of the entry it was built from.
Only ever kept in memory: writing hashes of the secrets to disk would
hand an attacker something to crack offline.")

  (defvar org-password-manager--store-timer nil)

  (defun org-password-manager--slug (string)
    "Turn STRING into something usable as a single filename component."
    (let ((slug (replace-regexp-in-string
                 "\\s-+" " "
                 (replace-regexp-in-string "[/\0]" "-" (string-trim string)))))
      ;; A leading dot would hide the entry from browserpass.
      (setq slug (replace-regexp-in-string "\\`\\.+" "" slug))
      (truncate-string-to-width (if (string-empty-p slug) "sans-titre" slug) 80)))

  (defun org-password-manager--domain-p (string)
    "Whether STRING is a full domain name, optionally followed by a port.
Browserpass only auto-matches a site when such a name appears in the
entry path, so anything else is better left at the root of the store:
`HOST' here also holds the likes of \"NickServ\" or a bare IP."
    (and string
         (string-match-p "\\`[A-Za-z0-9-]+\\(\\.[A-Za-z0-9-]+\\)*\\.[A-Za-z]\\{2,\\}\\(:[0-9]+\\)?\\'"
                         string)))

  (defun org-password-manager--store-host ()
    "Domain to file the entry at point under, or nil to keep it at the root."
    (let* ((url (org-entry-get (point) "URL"))
           (candidate (or (and url (url-host (url-generic-parse-url url)))
                          (org-entry-get (point) "HOST")
                          (org-get-heading t t t t))))
      (and (org-password-manager--domain-p candidate) candidate)))

  (defun org-password-manager--store-entries ()
    "Return (RELPATH . CONTENT) for each entry of the current buffer to export."
    (let ((taken (make-hash-table :test 'equal)))
      (delq nil
            (org-map-entries
             (lambda ()
               (let ((heading (substring-no-properties
                               (org-link-display-format (org-get-heading t t t t))))
                     (top (car (org-get-outline-path)))
                     (password (org-entry-get (point) "PASSWORD"))
                     (login (or (org-entry-get (point) "LOGIN")
                                (org-entry-get (point) "USER")
                                (org-entry-get (point) "NAME")
                                (org-entry-get (point) "EMAIL")))
                     (url (org-entry-get (point) "URL"))
                     (host (org-password-manager--store-host)))
                 (unless (or (null password)
                             (member top org-password-manager-store-exclude)
                             (and org-password-manager-store-require-url (null url)))
                   (let* ((name (org-password-manager--slug heading))
                          (base (if host
                                    (concat (org-password-manager--slug host) "/" name)
                                  name))
                          (relpath (concat base ".gpg"))
                          (n 1))
                     ;; Two entries can legitimately share a heading.
                     (while (gethash relpath taken)
                       (setq relpath (format "%s (%d).gpg" base (setq n (1+ n)))))
                     (puthash relpath t taken)
                     (cons relpath
                           (concat password "\n"
                                   (and login (format "login: %s\n" login))
                                   (and url (format "url: %s\n" url))))))))
             (concat "PASSWORD" "={.+}") 'file))))

  (defun org-password-manager--store-guard (directory)
    "Create DIRECTORY if missing, and refuse to touch a store we did not write."
    (let ((marker (expand-file-name org-password-manager-store-marker directory)))
      (cond ((not (file-directory-p directory))
             (make-directory directory t)
             (set-file-modes directory #o700)
             (write-region "Generated from personnel.org.gpg -- do not edit by hand.\n"
                           nil marker nil 'silent))
            ((file-exists-p marker))
            (t (error "%s exists but holds no %s marker; refusing to overwrite it"
                      directory org-password-manager-store-marker)))))

  (defun org-password-manager--store-write (content file recipients)
    "Encrypt CONTENT to RECIPIENTS into FILE.
Deliberately not `epg-encrypt-string': EPG drives gpg through
`--command-fd' and relays its questions to the minibuffer, which would
wedge Emacs on a prompt nobody is watching when this runs from a save
hook.  Plain `--batch' gpg cannot ask anything.  The plaintext goes
down a pipe, so it never reaches the disk in the clear."
    (make-directory (file-name-directory file) t)
    (let ((process
           (make-process
            :name "org-password-manager-gpg"
            :buffer nil
            :noquery t
            :connection-type 'pipe
            :coding '(binary . binary)
            :command (append (list epg-gpg-program
                                   "--batch" "--yes" "--quiet"
                                   "--trust-model" "always"
                                   "--output" (expand-file-name file)
                                   "--encrypt")
                             (mapcan (lambda (r) (list "--recipient" r))
                                     (copy-sequence recipients))))))
      (process-send-string process content)
      (process-send-eof process)
      (while (process-live-p process)
        (accept-process-output process 0.05))
      (unless (eq (process-exit-status process) 0)
        (error "gpg a échoué (code %s) sur %s"
               (process-exit-status process) file))
      (set-file-modes file #o600)))

  (defun org-password-manager--store-prune (directory wanted)
    "Delete every .gpg under DIRECTORY absent from the WANTED hash table."
    (let ((removed 0))
      (dolist (file (directory-files-recursively directory "\\.gpg\\'"))
        (let ((relpath (file-relative-name file directory)))
          (unless (gethash relpath wanted)
            (delete-file file)
            (remhash relpath org-password-manager--store-hashes)
            (setq removed (1+ removed)))))
      ;; Sweep the directories the deletions emptied.
      (dolist (dir (nreverse (directory-files-recursively directory "" t nil t)))
        (when (and (file-directory-p dir)
                   (null (directory-files dir nil directory-files-no-dot-files-regexp t)))
          (delete-directory dir)))
      removed))

  (defun org-password-manager-export-store (&optional force)
    "Regenerate the browserpass store from `org-password-manager-file'.
Only entries whose content changed are re-encrypted; with a prefix
argument FORCE, rewrite every entry."
    (interactive "P")
    (let* ((directory (expand-file-name org-password-manager-store-directory))
           (buffer (find-file-noselect org-password-manager-file))
           (recipients (buffer-local-value 'epa-file-encrypt-to buffer))
           (entries (with-current-buffer buffer (org-password-manager--store-entries)))
           (wanted (make-hash-table :test 'equal))
           (written 0))
      (unless recipients
        (error "No `epa-file-encrypt-to' set in %s" org-password-manager-file))
      ;; Fail loudly here rather than once per entry.
      (dolist (recipient recipients)
        (unless (eq 0 (apply #'call-process epg-gpg-program nil nil nil
                             (list "--batch" "--list-keys" recipient)))
          (error "No public key found for %s" recipient)))
      (org-password-manager--store-guard directory)
      (when force (clrhash org-password-manager--store-hashes))
      ;; Written for the sake of `pass' itself, browserpass does not need it.
      (write-region (concat (mapconcat #'identity recipients "\n") "\n") nil
                    (expand-file-name ".gpg-id" directory) nil 'silent)
      (dolist (entry entries)
        (let* ((relpath (car entry))
               (content (cdr entry))
               (file (expand-file-name relpath directory))
               (hash (secure-hash 'sha256 content)))
          (puthash relpath t wanted)
          (unless (and (equal hash (gethash relpath org-password-manager--store-hashes))
                       (file-exists-p file))
            (org-password-manager--store-write content file recipients)
            (puthash relpath hash org-password-manager--store-hashes)
            (setq written (1+ written)))))
      (let ((removed (org-password-manager--store-prune directory wanted)))
        (message "password-store: %d entrée(s) exportée(s), %d écrite(s), %d supprimée(s)"
                 (length entries) written removed))))

  (defun org-password-manager--store-export-quietly ()
    (condition-case err
        (org-password-manager-export-store)
      (error (message "password-store: export échoué: %s"
                      (error-message-string err)))))

  (defun org-password-manager--store-schedule-export ()
    "Regenerate the store shortly after saving, so the save itself stays snappy."
    (when (and org-password-manager-store-auto
               buffer-file-name
               (file-equal-p buffer-file-name
                             (expand-file-name org-password-manager-file)))
      (when (timerp org-password-manager--store-timer)
        (cancel-timer org-password-manager--store-timer))
      (setq org-password-manager--store-timer
            (run-with-idle-timer 1 nil #'org-password-manager--store-export-quietly))))

  (add-hook 'after-save-hook #'org-password-manager--store-schedule-export)

  (defun org-password-manager-set-url (url)
    "Set the URL property of the entry at point.
That is what lets browserpass match the entry against a site
automatically, since it looks for a domain in the entry path."
    (interactive (list (read-string "URL: " (org-entry-get (point) "URL"))))
    (org-entry-put (point) "URL" url))

  (defun org-password-manager--read-url-table (file)
    "Parse FILE, mapping entry headings to domains.
Lines are `heading TAB domain'; blank lines and `#' comments are skipped,
as are lines left without a domain."
    (let ((table (make-hash-table :test 'equal)))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
            (unless (or (string-prefix-p "#" line)
                        (string-empty-p (string-trim line)))
              (let ((heading (string-trim (or (nth 0 (split-string line "\t")) "")))
                    (domain (string-trim (or (nth 1 (split-string line "\t")) ""))))
                (unless (or (string-empty-p heading) (string-empty-p domain))
                  (puthash heading domain table)))))
          (forward-line 1)))
      table))

  (defun org-password-manager-apply-urls (file)
    "Give entries an URL property from the table in FILE.
Entries that already carry an URL are left alone.  The org buffer is
left modified but unsaved, so that the changes can be reviewed; saving
is what regenerates the store."
    (interactive "fTableau titre/domaine: ")
    (let ((table (org-password-manager--read-url-table file))
          (seen (make-hash-table :test 'equal))
          (added 0) (already 0))
      (with-current-buffer (find-file-noselect org-password-manager-file)
        (org-map-entries
         (lambda ()
           (let* ((heading (substring-no-properties
                            (org-link-display-format (org-get-heading t t t t))))
                  (domain (gethash heading table)))
             (when domain
               (puthash heading t seen)
               (if (org-entry-get (point) "URL")
                   (setq already (1+ already))
                 ;; Headings are not unique; every entry sharing one gets it.
                 (org-entry-put (point) "URL" (format "https://%s/" domain))
                 (setq added (1+ added))))))
         (concat "PASSWORD" "={.+}") 'file)
        (let ((missing 0))
          (maphash (lambda (heading _) (unless (gethash heading seen)
                                         (setq missing (1+ missing))))
                   table)
          (message (concat "URL posées sur %d entrée(s), %d avaient déjà une URL, "
                           "%d titre(s) du tableau introuvable(s). "
                           "Relis puis sauve pour régénérer le store.")
                   added already missing))))))

(provide 'init-password)
