(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/org-mode/lisp"))
(require 'org)

(add-to-list 'load-path "~/.emacs.d/site-lisp/org-mode/contrib/lisp")
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

;; set org id locations
(setq org-id-locations-file "~/Dropbox/Org/.org-id-locations")

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
      '(("\\`file:.*/\\([^/:]+\\)\\(::.*\\)" . "\\1")
        ("\\`file:.*/\\([^/:]+\\)" . "\\1")))

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
      '(("b" "Thesis Work" tags-todo "boss")
        ("t" "All TODO" alltodo "")))

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

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c L") 'org-insert-link-global)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)

(setq org-agenda-files
      '("~/Dropbox/Org/agenda.org"
        "~/Dropbox/Org/someday.org"
        "~/Dropbox/Org/specialdays.org"
        "~/Dropbox/Org/books.org"))

;; don't wait for agenda to be opened to update appt
(org-agenda-to-appt 1)

(setq french-holiday
      '((holiday-fixed 1 1 "Jour de l'an")
        (holiday-fixed 5 1 "Fête du travail")
        (holiday-fixed 5 8 "Victoire 45")
        (holiday-fixed 7 14 "Fête nationale")
        (holiday-fixed 8 15 "Assomption")
        (holiday-fixed 11 1 "Toussaint")
        (holiday-fixed 11 11 "Armistice 1918")
        (holiday-fixed 12 25 "Noël")
        (holiday-easter-etc 1 "Lundi de Pâques")
        (holiday-easter-etc 39 "Ascension")
        (holiday-easter-etc -47 "Mardi gras")
        (holiday-easter-etc 50 "Lundi de Pentecôte")))


;; (holiday-float 6 0 3 "Fête des pères")
;; (if (equal (holiday-float 5 0 -1 "") (holiday-easter-etc 49 ""))
;;     (holiday-float 6 0 1 "Fête des mères!!")
;;   (holiday-float 5 0 -1 "Fête des mères!!"))

(setq calendar-date-style 'european
      calendar-holidays french-holiday
      calendar-mark-holidays-flag t)

;; warning with appt and notify
(setq
 appt-message-warning-time 15 ;; warn 15 min in advance
 appt-display-interval 3
 appt-display-mode-line t     ;; show in the modeline
 appt-display-format 'window) ;; use our func

(appt-activate 1)              ;; active appt (appointment notification)
(display-time)                 ;; time display is required for this...

;; update appt each time agenda is opened, force a refresh to cancel
;; deleted appt
(add-hook 'org-finalize-agenda-hook
          (lambda () (org-agenda-to-appt 1)))

;; our little façade-function for djcb-popup
(defun appt-display (mins current-time msgs)
  (mapcar*
   (lambda (min msg)
     (notifications-notify
      :title (concat "Appointment "
                     (cond
                      ((equal min "0")
                       "now!")
                      ((equal min "1")
                       "in 1 minute!")
                      (t
                       (format "in %s minutes" min))))
      :body msg
      :app-icon "~/.emacs.d/icons/appointment-soon.png"
      :sound-file "/usr/share/sounds/ubuntu/stereo/phone-incoming-call.ogg"))
   (if (listp mins) mins (list mins))
   (if (listp msgs) msgs (list msgs))))

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

;; open pdf files with acroread or evince (fuck gv)
(and (executable-find "acroread")
     (push (cons "pdf" "acroread %s") org-file-apps))
(and (executable-find "evince")
     (push (cons "pdf" "evince %s") org-file-apps))

;; open ods files with libreoffice
(add-to-list 'org-file-apps '("\\.od[st]\\'" . "soffice %s"))

;; bigger latex fragment
(plist-put org-format-latex-options :scale 1.5)

(defun jump-to-org-agenda ()
  (interactive)
  (let ((buf (or (get-buffer "*Org Agenda*")
                 (get-buffer "*Org Agenda(a)*")))
        wind)
    (if buf
        (if (setq wind (get-buffer-window buf))
            (when (called-interactively-p 'any)
              (select-window wind)
              (org-fit-window-to-buffer))
          (if (called-interactively-p 'any)
              (progn
                (select-window (display-buffer buf t t))
                (org-fit-window-to-buffer))
            (with-selected-window (display-buffer buf)
              (org-fit-window-to-buffer))))
      (call-interactively 'org-agenda-list))))

(run-with-idle-timer 900 t 'jump-to-org-agenda)

;; (add-to-list  'load-path "~/.emacs.d/site-lisp/google-weather-el")
;; (setq url-cache-directory "~/.emacs.d/cache")
;; (require 'org-google-weather)

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

;; Calcul de Pâques (from holidays.el)
(defun abs-easter ()
  (let* ((displayed-year (caddr date))
         (century (1+ (/ displayed-year 100)))
         (shifted-epact ;; Age of moon for April 5...
          (% (+ 14 (* 11 (% displayed-year 19)) ;;     ...by Nicaean rule
                (- ;; ...corrected for the Gregorian century rule
                 (/ (* 3 century) 4))
                (/ ;; ...corrected for Metonic cycle inaccuracy.
                 (+ 5 (* 8 century)) 25)
                (* 30 century)) ;;              Keeps value positive.
             30))
         (adjusted-epact ;;  Adjust for 29.5 day month.
          (if (or (= shifted-epact 0)
                  (and (= shifted-epact 1) (< 10 (% displayed-year 19))))
              (1+ shifted-epact)
            shifted-epact))
         (paschal-moon ;; Day after the full moon on or after March 21.
          (- (calendar-absolute-from-gregorian (list 4 19 displayed-year))
             adjusted-epact)))
    (calendar-dayname-on-or-before 0 (+ paschal-moon 7))))

;; Jours fériés et fêtes
(defun paques-fetes ()
  (let ((abs-easter (abs-easter)))
    (and (calendar-date-equal
          date
          (calendar-gregorian-from-absolute (+ abs-easter -47)))
         "Mardi gras")))

(defun fete-meres ()
  (let ((abs-easter (abs-easter)))
    (or (and (diary-float 5 0 -1)
             (not
              (calendar-date-equal
               date
               (calendar-gregorian-from-absolute (+ (abs-easter) 49)))))
        (and (diary-float 6 0 1)
             (calendar-date-equal
              date
              (calendar-gregorian-from-absolute (+ (abs-easter) 56)))))))

(and (boundp 'server-process)
     (require 'org-protocol))

;; taken from worg
(defun org-remove-redundant-tags ()
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

(defun clean-org-buffer ()
  (when (eq major-mode 'org-mode)
    (and buffer-file-name
         (file-exists-p buffer-file-name)
         (org-remove-redundant-tags))
    (org-align-all-tags)
    (org-update-all-dblocks)))

(add-hook 'before-save-hook 'clean-org-buffer)

;; display agenda associated with file via org-context
(defun org-agenda-from-file (file key)
  (let* ((directory (directory-file-name
                     (file-name-directory file)))
         (org-agenda-custom-commands
          (org-context-agenda-expand
           (assoc-default (expand-file-name directory)
                          org-context-agenda-alist 'string-match)
           directory)))
    (and org-agenda-custom-commands
         (org-agenda nil key))))

;; don't warn when a link run `org-agenda-from-file'
(setq org-confirm-elisp-link-not-regexp
      "\\`(org-agenda-from-file \".*\" \"[a-zA-Z]+\")\\'")

;; enable sticky agenda to navigate between them
(eval-after-load "org-agenda"
  '(org-toggle-sticky-agenda 1))

;; update project cookie and look at specified org file
(defun org-update-project-cookies (n-done n-not-done)
  (let ((project (org-entry-get (point) "PROJECT"))
        (n-done 0)
        (n-todo 0)
        todo-file key)
    (when (and project
               (setq todo-file (org-entry-get (point) "TODOFILE"))
               (setq key (org-entry-get (point) "KEY"))
               (file-exists-p todo-file))
      (with-current-buffer (find-file-noselect
                            (make-temp-file nil nil ".org"))
        (insert-file-contents todo-file)
        (org-map-entries
         (lambda ()
           (when (looking-at org-complex-heading-regexp)
             (cond ((member (match-string-no-properties 2)
                            org-done-keywords)
                    (setq n-done (1+ n-done)))
                   ((member (match-string-no-properties 2)
                            org-todo-keywords)
                    (setq n-todo (1+ n-todo)))))))
        (erase-buffer)
        (set-buffer-modified-p nil)
        (kill-buffer))
      (save-excursion
        (org-back-to-heading t)
        (when (looking-at org-complex-heading-regexp)
          (replace-match
           (format "[%d/%d] [[elisp:(org-agenda-from-file \"%s\" \"%s\")][%s]]"
                   n-done (+ n-done n-todo) todo-file key project)
           nil nil nil 4))))))

(add-hook 'org-after-todo-statistics-hook 'org-update-project-cookies)

(defvar org-projects-refile-targets)

;; Add all todo.org files from Projects headline in someday.org as targets
(find-file "~/Dropbox/Org/someday.org")

;; helm completion enabled in my patched org
(if (boundp 'org-completion-handler)
    (setq org-completion-handler 'helm))


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

(setq org-refile-targets
      `((,org-agenda-files . (:level . 1))
        (,org-projects-refile-targets . (:level . 0))))

(setq org-refile-use-outline-path 'full-file-path)

(setq org-outline-path-complete-in-steps nil)

;; custom export from tsv to timestamp to be placed in agenda
(defun orgtbl-to-agenda (table params)
  (let ((table-strip
         (delq nil
               (mapcar (lambda (row)
                         (if (listp row)
                             (list (nth 1 row) (nth 2 row) (nth 5 row))))
                       table))))
    (mapconcat
     (lambda (row)
       (format "*** %s %s %s"
               (format-time-string
                (car org-time-stamp-formats)
                (apply 'encode-time  0 0 0
                       (mapcar 'string-to-number (split-string (nth 0 row) "/"))))
               (nth 2 row) (nth 1 row)))
     table-strip
     "\n")))

;; Taken from http://sachachua.com/blog/2013/01/emacs-org-task-related-keyboard-shortcuts-agenda/
(defun org-agenda-mark-done-and-add-followup ()
  "Mark the current TODO as done and add another task after it.
Creates it at the same level as the previous task, so it's better to use
this with to-do items than with projects or headings."
  (interactive)
  (org-agenda-todo "DONE")
  (org-agenda-switch-to)
  (org-capture 0 "t"))

;; Override the key definition
(define-key org-agenda-mode-map "X" 'org-agenda-mark-done-and-add-followup)

(provide 'init-org)
