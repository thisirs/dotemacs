(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/org-mode/lisp"))
(require 'org)

(add-to-list 'load-path "~/.emacs.d/vendor/org-mode/contrib/lisp")
(require 'org-drill)

;; workaround to use yassnippet in org-mode
(defun yas/org-very-safe-expand ()
  (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

(add-hook 'org-mode-hook
          (lambda ()
            ;; yasnippet (using the new org-cycle hooks)
            (make-variable-buffer-local 'yas/trigger-key)
            (setq yas/trigger-key [tab])
            (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
            (define-key yas/keymap [tab] 'yas/next-field-or-maybe-expand)))

(setq org-todo-keywords
      '("TODO" "|" "CANCELLED" "DONE"))

;; no recursive todo in agenda
(setq org-agenda-todo-list-sublevels nil)

;; ignore scheduled and deadline
(setq org-agenda-todo-ignore-scheduled t)
(setq org-agenda-todo-ignore-deadlines t)

;; remove tags in agenda
(setq org-agenda-remove-tags t)

;; special navigation in org mode
(setq org-special-ctrl-a/e t)

;; fontify src blocks
(setq org-src-fontify-natively t)

;; restore windows configuration
(setq org-agenda-restore-windows-after-quit t)

(defun todo-item ()
  "Auto insert link when capturing if point is on a TODO line."
  (or
   (with-current-buffer (org-capture-get :original-buffer)
     (and (buffer-file-name)
          (save-excursion
            (beginning-of-line)
            (when (looking-at
                   (concat "[\t ]*"
                           (regexp-quote (or comment-start ""))
                           "[\t ]+TODO:?[\t ]+"))
              (goto-char (match-end 0))
              (let* ((txt (buffer-substring (point) (line-end-position)))
                     (search (org-make-org-heading-search-string
                              (buffer-substring (line-beginning-position)
                                                (line-end-position))))
                     (link (concat "file:" (abbreviate-file-name buffer-file-name)
                                   "::" search)))
                (org-make-link-string link txt))))))
   ""))

;; shorter description
(setq org-link-to-description
      '(("\\`file:.*/\\([^/:]+\\)" . "\\1")))

(setq org-make-link-description-function
      (lambda (link description)
        (let ((found (assoc-default link org-link-to-description 'string-match)))
          (cond
           ((stringp found) (match-substitute-replacement found t nil link))))))

(defun org-string-to-link (file)
  "Command that make the selected region an org link pointing to
the selected file."
  (interactive "fFile: ")
  (insert (org-make-link-string
           file
           (if (file-directory-p file)
               (file-name-as-directory file)
             (file-name-nondirectory file)))))

;; function to replace the description in a link
(defun org-replace-name-in-link (link &optional here)
  (setq here (or here "here"))
  (if (string-match "\\[\\(\\[.*?\\]\\)\\(\\[.*?\\]\\)?\\]" link)
      (replace-match (concat "[\\1[" here "]]") nil nil link)
    link))



;; return stored link with description "here"
(defun my-name ()
  (file-name-nondirectory
   (replace-regexp-in-string
    "%" "\\%"
    (org-link-unescape (or (plist-get org-store-link-plist :link) "")))))

;; fill :initial prop
(defun initial-prop-filled ()
  (with-temp-buffer
    (insert (or (plist-get org-store-link-plist :initial) ""))
    (fill-region (buffer-end 0) (buffer-end 1))
    (buffer-string)))

;; load templates from personal location
(setq org-capture-templates
      (load-file-to-list "~/Dropbox/emacs/org-capture-templates.el"))

(defun add-days (date1 days)
  "Add `days' days to `date'"
  (decode-time
   (time-add
    (apply 'encode-time (org-parse-time-string date1))
    (days-to-time days))))

(defun deadline-from-now (days &optional deadline)
  "Construit la date de retour avec une deadline"
  (let ((time (format-time-string
               (car org-time-stamp-formats)
               (time-add (current-time) (days-to-time days)))))
    (if (integerp deadline)
        (concat
         (substring time 0 -1)
         " -" (format "%d" deadline) "d>")
      time)))

(define-key global-map "\C-cc" 'org-capture)

;; custom agenda view
(setq org-agenda-custom-commands
      '(("b" "Thesis Work" tags-todo "boss")))


;; icons in agenda
(setq org-agenda-category-icon-alist
      '(("Emacs" "/usr/local/share/icons/hicolor/16x16/apps/emacs.png" nil nil :ascent center)
        ("Books\\|Magazines" "~/.emacs.d/icons/book.png" nil nil :ascent center)
        ("Anniv" "~/.emacs.d/icons/birthday.png" nil nil :ascent center)
        ("Fête" "~/.emacs.d/icons/party-hat.png" nil nil :ascent center)
        ("Férié" "~/.emacs.d/icons/flip_flops.png" nil nil :ascent center)
        ("Schlink!" "~/.emacs.d/icons/euro.png" nil nil :ascent center)
        ("Santé" "~/.emacs.d/icons/syringe.png" nil nil :ascent center)
        ("Download" "~/.emacs.d/icons/download.png" nil nil :ascent center)
        ("" '(space . (:height (16) :width (16))))))

(setq org-agenda-day-face-function
      (defun jd:org-agenda-day-face-holidays-function (date)
        "Compute DATE face for holidays."
        (unless (org-agenda-todayp date)
          (dolist (file (org-agenda-files nil 'ifmode))
            (let ((face
                   (dolist (entry (org-agenda-get-day-entries file date))
                     (let ((category (with-temp-buffer
                                       (insert entry)
                                       (org-get-category (point-min)))))
                       (when (string= "Vacances" category)
                         (return 'org-agenda-date-weekend))))))
              (when face (return face)))))))

;; annoted todo are stared
(eval-after-load "org-agenda"
  '(add-to-list 'org-agenda-prefix-format
                '(todo . " %(annotedp)%i %-12:c")))

(defun annotedp ()
  (or
   (and (boundp 'beg) (boundp 'end)
        (save-excursion
          (goto-char beg)
          (if (re-search-forward "- Note taken" end t) "*")))
   " "))


;; logging
(setq org-log-done 'time)

(setq org-agenda-skip-deadline-if-done t)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-agenda-files
      '("~/Dropbox/Org/agenda.org"
        "~/Dropbox/Org/someday.org"
        "~/Dropbox/Org/specialdays.org"
        "~/Dropbox/Org/books.org"))

(setq calendar-holidays
      '((holiday-fixed 1 1 "Nouvel an")
        (holiday-fixed 5 1 "Fête du travail")
        (holiday-fixed 5 8 "Victoire 1945")
        (holiday-fixed 7 14 "Fête nationale")
        (holiday-fixed 8 15 "Assomption")
        (holiday-fixed 11 11 "Armistice 1918")
        (holiday-fixed 11 1 "Toussaint")
        (holiday-fixed 12 25 "Noël")
        (holiday-float 5 0 -1 "Fête des mères")
        (holiday-float 6 0 3 "Fête des pères")))

(setq calendar-mark-holidays-flag t)

;; warning with appt and notify
(setq
 appt-message-warning-time 15 ;; warn 15 min in advance
 appt-display-interval 1
 appt-display-mode-line t     ;; show in the modeline
 appt-display-format 'window) ;; use our func

(appt-activate 1)              ;; active appt (appointment notification)
(display-time)                 ;; time display is required for this...

;; update appt each time agenda opened

(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)

;; our little façade-function for djcb-popup
(defun appt-display (min-to-app current-time msg)
  (notifications-notify
   :title (format "Appointment in %s minute%s" min-to-app
                  (if (> min-to-app 1) "s" ""))
   :body msg
   :app-icon "/usr/share/icons/gnome/32x32/status/appointment-soon.png"
   :sound-file "/usr/share/sounds/ubuntu/stereo/phone-incoming-call.ogg"))

(setq appt-disp-window-function 'appt-display)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (latex . t) ; this is the entry to activate LaTeX
   (sh . t)
   (matlab . t)
   (ruby . t)))

(setq org-babel-sh-command "bash")

;; bib citations in org files
(defun org-mode-reftex-setup ()
  (reftex-mode t)
  (when (and (buffer-file-name)
             (file-exists-p (buffer-file-name)))
    (reftex-parse-all)
    (reftex-set-cite-format "[[note::%l][%l]]")
    (define-key org-mode-map (kbd "C-c )") 'reftex-citation)))

(add-hook 'org-mode-hook 'org-mode-reftex-setup)

;; open pdf files with acroread
(and (executable-find "acroread")
     (push (cons "pdf" "acroread %s") org-file-apps))

;; bigger latex fragment
(plist-put org-format-latex-options :scale 1.5)

(defun jump-to-org-agenda ()
  (interactive)
  (let ((buf (get-buffer "*Org Agenda*"))
        wind)
    (if buf
        (if (setq wind (get-buffer-window buf))
            (select-window wind)
          (if (called-interactively-p)
              (progn
                (select-window (display-buffer buf t t))
                (org-fit-window-to-buffer)
                ;; (org-agenda-redo)
                )
            (with-selected-window (display-buffer buf)
              (org-fit-window-to-buffer)
              ;; (org-agenda-redo)
              )))
      (call-interactively 'org-agenda-list)))
  ;;(let ((buf (get-buffer "*Calendar*")))
  ;;  (unless (get-buffer-window buf)
  ;;    (org-agenda-goto-calendar)))
  )

(run-with-idle-timer 900 t 'jump-to-org-agenda)

(add-to-list  'load-path "~/.emacs.d/vendor/google-weather-el")
(setq url-cache-directory "~/.emacs.d/cache")
(require 'org-google-weather)

(eval-after-load 'parse-time
  '(progn
     (setq parse-time-months
           (append parse-time-months
                   '(("janv" . 1)
                     ("jan" . 1)
                     ("févr" . 2)
                     ("fév" . 2)
                     ("fev" . 2)
                     ("mar" . 3)
                     ("avril" . 4)
                     ("avr" . 4)
                     ("mai" . 5)
                     ("jun" . 6)
                     ("juill" . 7)
                     ("jul" . 7)
                     ("aou" . 8)
                     ("sept" . 9)
                     ("sep" . 9)
                     ("oct" . 10)
                     ("nov" . 11)
                     ("déc" . 12)
                     ("dec" . 12)
                     ("janvier" . 1)
                     ("février" . 2)
                     ("mars" . 3)
                     ("avril" . 4)
                     ("juin" . 6)
                     ("juillet" . 7)
                     ("aout" . 8)
                     ("août" . 8)
                     ("septembre" . 9)
                     ("octobre" . 10)
                     ("novembre" . 11)
                     ("décembre" . 12))))
     (setq parse-time-weekdays
           (append parse-time-weekdays
                   '(("dim" . 0) ("lun" . 1) ("mar" . 2)
                     ("mer" . 3) ("jeu" . 4) ("ven" . 5)
                     ("sam" . 6) ("dimanche" . 0) ("lundi" . 1)
                     ("mardi" . 2) ("mercredi" . 3)
                     ("jeudi" . 4) ("vendredi" . 5)
                     ("samedi" . 6))))))

(and (boundp 'server-process)
     (require 'org-protocol))

;; taken from worg
(defun dmj/org-remove-redundant-tags ()
  "Remove redundant tags of headlines in current buffer.

A tag is considered redundant if it is local to a headline and
inherited by a parent headline."
  (interactive)
  (when (eq major-mode 'org-mode)
    (save-excursion
      (org-map-entries
       (lambda ()
         (let ((alltags (split-string (or (org-entry-get (point) "ALLTAGS") "") ":"))
               local inherited tag)
           (dolist (tag alltags)
             (if (get-text-property 0 'inherited tag)
                 (push tag inherited) (push tag local)))
           (dolist (tag local)
             (if (member tag inherited) (org-toggle-tag tag 'off)))))
       t nil))))

(add-hook 'before-save-hook
          (lambda ()
            (when (eq major-mode 'org-mode)
              (and buffer-file-name
                   (file-exists-p buffer-file-name)
                   (dmj/org-remove-redundant-tags))
              (org-align-all-tags)
              (org-update-all-dblocks))))

;; display agenda associated with file via org-context
(defun org-agenda-from-file (file)
  (let ((org-agenda-custom-commands
         (assoc-default (expand-file-name (directory-file-name
                                           (file-name-directory file)))
                        org-context-agenda-alist 'string-match)))
    (and org-agenda-custom-commands
         (org-agenda nil "c"))))

;; don't warn when a link run `org-agenda-from-file'
(setq org-confirm-elisp-link-not-regexp "(org-agenda-from-file \".*\")")

;; enable sticky agenda to navigate between them
(eval-after-load "org-agenda"
  '(org-toggle-sticky-agenda 1))

;; update project cookie and look at specified org file
(defun org-update-project-cookies (n-done n-not-done)
  (let ((project (org-entry-get (point) "PROJECT"))
        todo-file tab n-done n-total)
    (when (and project
               (setq todo-file (org-entry-get (point) "TODOFILE"))
               (file-exists-p todo-file))
      (with-current-buffer (find-file-noselect
                            (make-temp-file nil nil ".org"))
        (insert-file-contents todo-file)
        (setq tab (org-map-entries
                   (lambda ()
                     (when (looking-at org-complex-heading-regexp)
                       (member (match-string 2) org-done-keywords))))
              n-done (length (delq nil tab))
              n-total (length tab))
        (erase-buffer)
        (set-buffer-modified-p nil)
        (kill-buffer))
      (save-excursion
        (org-back-to-heading t)
        (when (looking-at org-complex-heading-regexp)
          (replace-match (format "[%d/%d] [[elisp:(org-agenda-from-file \"%s\")][%s]]"
                                 n-done n-total todo-file project)
                         nil nil nil 4))))))

(add-hook 'org-after-todo-statistics-hook 'org-update-project-cookies)

(defvar org-projects-refile-targets)

(find-file "~/Dropbox/Org/someday.org")

(setq org-projects-refile-targets
      (with-current-buffer "someday.org"
        (save-excursion
          (goto-char (point-min))
          (if (re-search-forward
               (format org-complex-heading-regexp-format "Projects")
               nil t)
              (let (project-files project-file)
                (org-map-entries
                 (lambda ()
                   (setq project-file (org-entry-get (point) "TODOFILE"))
                   (if (and (stringp project-file)
                            (file-exists-p project-file))
                       (setq project-files (cons project-file project-files))))
                 nil 'tree)
                project-files)))))

;; to be able to refile to projects files
(setq org-refile-targets
      `((,(append org-agenda-files org-projects-refile-targets) . (:level . 1))))

(setq org-refile-use-outline-path 'file)
(setq org-refile-use-outline-path 'full-file-path)

(setq org-outline-path-complete-in-steps nil)


(provide 'init-org)
