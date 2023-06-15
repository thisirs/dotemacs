;; Sync org files with external calendar through CalDAV
(use-package org-caldav)

(use-package org-attach
  :straight nil
  :custom (org-attach-archive-delete t))

;; https://github.com/emacsattic/org-link-minor-mode
(use-package org-link-minor-mode) ;; Enable org-mode links in non-org modes

(use-package ox-koma-letter
  :straight nil
  :custom
  ;; email is specified is lco file
  (org-koma-letter-email nil)

  ;; no backaddress by default
  (org-koma-letter-use-backaddress nil)

  ;; no foldmarks by default
  (org-koma-letter-use-foldmarks nil))

(use-package org        ; Outline-based notes management and organizer
  :init
  (defun org-save-all-agenda-buffers ()
    "Save all agenda buffers without user confirmation."
    (interactive)
    (message "Saving all agenda buffers...")
    (save-some-buffers t (lambda () (org-agenda-file-p (buffer-file-name))))
    (message "Saving all agenda buffers... done"))

  (defun org-force-auto-fill ()
    "Make `comment-auto-fill-only-comments' buffer-local and set it to nil."
    (when comment-auto-fill-only-comments
      (set (make-local-variable 'comment-auto-fill-only-comments) nil))
    (auto-fill-mode +1))

  (defun turn-off-truncate-lines ()
    (cl-letf (((symbol-function 'message)
               (lambda (&rest args) t)))
      (toggle-truncate-lines -1)))

  :hook
  (auto-save-hook . org-save-all-agenda-buffers)
  (org-mode-hook . org-force-auto-fill)
  (org-mode-hook . turn-off-truncate-lines)

  :custom
  ;; Adapt indentation to outline node level
  (org-adapt-indentation t)

  (org-startup-folded t)

  ;; Archive subtrees by year
  (org-archive-location (format "%%s_archive_%s::" (format-time-string "%Y")))

  (org-todo-keywords '((sequence "TODO" "|" "CANCELLED" "DONE")))

  (org-use-fast-todo-selection t)

  ;; Allow property inheritance for org-context agenda in my scripts
  ;; directory
  (org-use-property-inheritance t)

  (org-tag-alist '(("home" . ?h)
                   ("labo" . ?l)))

  ;; Special navigation in org mode
  (org-special-ctrl-a/e t)

  ;; Smart C-k
  (org-special-ctrl-k t)

  ;; Logging
  (org-log-done 'time)

  :config

  ;; Custom datetimes in overlay when choosing a datetime
  (defun org-read-date-display-advice (oldfun)
    (let ((system-time-locale "fr_FR.utf8")
          (org-display-custom-times t)
          (org-timestamp-custom-formats '("%A %d %B %Y" . "%A %d %B %Y %H:%M")))
      (funcall oldfun)))

  (advice-add 'org-read-date-display :around #'org-read-date-display-advice)

  ;; Set org id locations
  (setq org-id-locations-file "~/SynologyDrive/Sylvain/Org/.org-id-locations")

  ;; Shorter description
  (setq org-link-to-description
        '(("\\`file:.*/\\([^/:]+\\)\\(::.*\\)" . "\\1")
          ("\\`file:.*/\\([^/:]+\\)" . "\\1")))

  (setq org-link-make-description-function
        (lambda (link description)
          (let ((found (assoc-default link org-link-to-description 'string-match)))
            (cond
             ((stringp found) (match-substitute-replacement found t nil link))))))

  (defun org-string-to-link (file)
    "Command that make the selected region an org link pointing to
the selected file."
    (interactive "fFile: ")
    (insert (org-link-make-string
             file
             (if (file-directory-p file)
                 (file-name-as-directory file)
               (file-name-nondirectory file)))))

  ;; Function to replace the description in a link
  (defun org-replace-name-in-link (link &optional here)
    (setq here (or here "here"))
    (if (string-match "\\[\\(\\[.*?\\]\\)\\(\\[.*?\\]\\)?\\]" link)
        (replace-match (concat "[\\1[" here "]]") nil nil link)
      link))

  ;; (setq org-agenda-day-face-function
  ;;       (defun jd:org-agenda-day-face-holidays-function (date)
  ;;         "Compute DATE face for holidays."
  ;;         (unless (org-agenda-todayp date)
  ;;           (dolist (file (org-agenda-files nil 'ifmode))
  ;;             (let ((face
  ;;                    (dolist (entry (org-agenda-get-day-entries file date))
  ;;                      (let ((category (with-temp-buffer
  ;;                                        (insert entry)
  ;;                                        (org-get-category (point-min)))))
  ;;                        (when (string= "Vacances" category)
  ;;                          (return 'org-agenda-date-weekend))))))
  ;;               (when face (return face)))))))

  ;; Calcul de Pâques (from holidays.el)
  (defvar date)

  ;; Adapted from holidays.el
  (defun abs-easter (year)
    "Return easter date for given YEAR."
    (let* ((century (1+ (/ year 100)))
           (shifted-epact ;; Age of moon for April 5...
            (% (+ 14 (* 11 (% year 19)) ;;     ...by Nicaean rule
                  (- ;; ...corrected for the Gregorian century rule
                   (/ (* 3 century) 4))
                  (/ ;; ...corrected for Metonic cycle inaccuracy.
                   (+ 5 (* 8 century)) 25)
                  (* 30 century)) ;;              Keeps value positive.
               30))
           (adjusted-epact ;;  Adjust for 29.5 day month.
            (if (or (= shifted-epact 0)
                    (and (= shifted-epact 1) (< 10 (% year 19))))
                (1+ shifted-epact)
              shifted-epact))
           (paschal-moon ;; Day after the full moon on or after March 21.
            (- (calendar-absolute-from-gregorian (list 4 19 year))
               adjusted-epact)))
      (calendar-dayname-on-or-before 0 (+ paschal-moon 7))))

  (defun org-easter (&optional n mark)
    "Easter date with an optional number of shifting days N."
    (with-no-warnings
      (let* ((easter (abs-easter (caddr date)))
             (ddate (calendar-gregorian-from-absolute (+ easter n))))
        (and (calendar-date-equal date ddate)
             (cons mark mark)))))

  (defun fete-meres ()
    (let ((easter (abs-easter (caddr date))))
      (or (and (diary-float 5 0 -1)
               (not
                (calendar-date-equal
                 date
                 (calendar-gregorian-from-absolute (+ easter 49)))))
          (and (diary-float 6 0 1)
               (calendar-date-equal
                date
                (calendar-gregorian-from-absolute (+ easter 56)))))))

  ;; Don't wait for agenda to be opened to update appt
  (org-agenda-to-appt 1)


  ;; (holiday-float 6 0 3 "Fête des pères")
  ;; (if (equal (holiday-float 5 0 -1 "") (holiday-easter-etc 49 ""))
  ;;     (holiday-float 6 0 1 "Fête des mères!!")
  ;;   (holiday-float 5 0 -1 "Fête des mères!!"))

  ;; Update appt each time agenda is opened, force a refresh to cancel
  ;; deleted appt
  (add-hook 'org-finalize-agenda-hook
            (lambda () (org-agenda-to-appt 1)))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (latex . t)
     (shell . t)
     (matlab . t)
     (ruby . t)
     (python . t)
     (R . t)))

  ;; Bigger latex fragment
  (plist-put org-format-latex-options :scale 1.5)

  (and (boundp 'server-process)
       (require 'org-protocol))

  ;; Taken from worg
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
      (save-excursion
        (goto-char (point-min))
        (org-align-tags t))
      (org-update-all-dblocks)))

  (add-hook 'before-save-hook #'clean-org-buffer)

  (defun org-toggle-timestamp (beg end)
    "Toggle all timestamps in region."
    (interactive "*r")
    (goto-char beg)
    (while (re-search-forward org-element--timestamp-regexp end t)
      (org-toggle-timestamp-type)))

  (defun org-context-capture-find-headline ()
    "Used with `org-context' in a capture template as a locating
function. Capture under a headline whose name is the file we
captured from."
    (let* ((file (org-capture-get :original-file))
           (base-dir (dir-locals-get-directory file))
           (rel (file-relative-name file base-dir))
           (name (if (string-match "^[^/]+" rel)
                     (match-string 0 rel)
                   (error "Unable to get name"))))
      (goto-char (point-min))
      (if (re-search-forward
           (format org-complex-heading-regexp-format
                   (regexp-quote (org-link-make-string (format "file:%s" name) name))) nil t)
          (progn
            (org-end-of-subtree t nil)
            (or (bolp) (insert "\n")))
        (goto-char (point-max))
        (or (bolp) (insert "\n"))
        (insert (format "\
* %s %s
  OPENED: %s
"
                        "ONPROGRESS"
                        (org-link-make-string (format "file:%s" name) name)
                        (format-time-string
                         "[%Y-%m-%d %a %H:%M:%S]"
                         (nth 6 (file-attributes (expand-file-name name base-dir))))))
        (org-id-get nil 'create))))

  (defun org-context-agenda-blocks (filev)
    "Construct a block agenda command where each block takes its
entry from each headline of FILEV."
    (with-current-buffer (find-file-noselect filev)
      (save-excursion
        (goto-char (point-min))
        (list "t" "Scripts TODO"
              (delq nil
                    (org-map-entries
                     (lambda ()
                       (when (looking-at org-complex-heading-regexp)
                         (when (= (length (match-string-no-properties 1)) 1)
                           (let ((header (match-string-no-properties 4))
                                 (todo (match-string-no-properties 2))
                                 (id (org-id-get)))
                             (if (member todo '("ONPROGRESS" "TODO" "NOTWORKING"))
                                 `(tags-todo ,(format "ID=\"%s\"" id)
                                             ((org-agenda-files (quote (,filev)))
                                              (org-agenda-overriding-header ,header))))))))))
              '((org-agenda-buffer-name "Projects TODO"))))))

  ;; Enable sticky agenda to navigate between them
  (eval-after-load "org-agenda"
    '(org-toggle-sticky-agenda -1))

  ;; Update project cookie and look at specified org file
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
             (format "[%d/%d] [[elisp:(org-context-agenda-from \"%s\" \"%s\")][%s]]"
                     n-done (+ n-done n-todo) todo-file key project)
             nil nil nil 4))))))

  (add-hook 'org-after-todo-statistics-hook #'org-update-project-cookies)

  (defvar org-other-files nil
    "List of org files other than agenda files destined to be
refile targets.")

  ;; First open someday.org and look for org files to add to
  ;; org-other-files in "Projects" headline.
  (find-file-noselect "~/SynologyDrive/Sylvain/Org/someday.org")

  (setq org-other-files
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

  (defun org-refile-all-org-files ()
    (seq-filter 'identity
                (seq-map (lambda (buf)
                           (with-current-buffer buf
                             (when (derived-mode-p 'org-mode)
                               (buffer-file-name buf))))
                         (buffer-list))))

  (setq org-refile-targets
        `((,org-agenda-files . (:level . 1))
          (org-refile-all-org-files . (:level . 0))))

  (setq org-refile-use-outline-path 'full-file-path)

  (setq org-outline-path-complete-in-steps nil)

  (declare-function org-table-check-inside-data-field "org-table")
  (define-key org-mode-map (kbd "C-c SPC")
    (lambda ()
      (interactive)
      (require 'org-table)
      (if (org-table-check-inside-data-field 'noerror)
          (org-table-blank-field)
        (call-interactively 'avy-goto-subword-1))))

  ;; Don't warn when a link run `org-agenda-from-file'
  (setq org-link-elisp-skip-confirm-regexp
        "\\`(org-context-agenda-from \".*\" \"[a-zA-Z]+\")\\'")

  (defun org-move-as-first-sibling ()
    "Move current subtree as first sibling."
    (interactive)
    (save-excursion
      (org-cut-subtree)
      (outline-up-heading 1)
      (forward-line)
      (org-paste-subtree))))


(use-package org-cite
  :straight nil
  :custom
  (org-cite-global-bibliography (list (expand-file-name "recherche/biblio/refs.bib" personal-directory)))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar))


(use-package ox-latex
  :straight nil
  :config
  ;; You need to install pygments to use minted
  (when (executable-find "pygmentize")
    (add-to-list 'org-latex-packages-alist '("" "minted"))
    (setq org-latex-listings 'minted)
    (setq org-latex-minted-options
          '(("mathescape" "true")
            ("linenos" "true")
            ("numbersep" "5pt")
            ("frame" "lines")
            ("framesep" "2mm")))
    (setq org-latex-pdf-process
          '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

    ;; Custom headline export for checklists, allow fake checkboxes in
    ;; headlines to be exported as well as those in lists. This can be
    ;; used with
    ;; #+LaTeX_HEADER: \renewcommand\labelitemi{}
    ;; to remove the bullets.
    (defun org-latex-format-headline-checkbox-function
        (todo todo-type priority text tags info)
      "Add a checkbox in front of headline depending of the states of
child checkboxes."
      (let ((checkbox ""))
        (when (boundp 'headline)
          (let ((all-checked t) (all-unchecked t) with-checkbox)
            (org-element-map headline 'item
              (lambda (element)
                (let ((state (plist-get (cadr element) :checkbox)))
                  (if state
                      (setq with-checkbox t))
                  (if (eq state 'on)
                      (setq all-unchecked nil))
                  (if (or (eq state 'off) (eq state 'trans))
                      (setq all-checked nil)))))
            (if with-checkbox
                (setq checkbox
                      (cond
                       (all-checked "$\\blacksquare$ ")
                       (all-unchecked "$\\Box$ ")
                       (t "$\\boxminus$ "))))))
        (concat
         (and todo (format "{\\bfseries\\sffamily %s} " todo))
         (and priority (format "\\framebox{\\#%c} " priority))
         checkbox
         text
         (and tags
              (format "\\hfill{}\\textsc{%s}" (mapconcat 'identity tags ":"))))))

    (setq org-latex-format-headline-function
          'org-latex-format-headline-checkbox-function)))

(use-package org-auto-archive
  :if (on-zbook)
  :straight `(org-auto-archive :type git
                               :local-repo ,(expand-file-name "org-auto-archive" projects-directory))
  :hook (kill-emacs-hook . org-auto-archive-process-buffers)
  :custom
  (org-auto-archive-plan
   '(("agenda.org" . ((subtree "External events")))
     ("agenda.org" . ((subtree "Evénements simples")))
     ("someday.org" . ((org-auto-archive-event-wait "1m")))
     ("agenda.org" . ((subtree "CID events")))))
  (org-auto-archive-handler-function 'org-auto-archive-handler-function-force))

(use-package org-capture
  :straight nil
  :preface (require 'init-org-capture-helpers)
  :custom
  ;; Load templates from personal location
  (org-capture-templates
   (load-file-to-list
    (expand-file-name "org-capture-templates.el" personal-emacs-directory))))

(use-package org-agenda
  :straight nil
  :init
  (defun annotedp ()
    (if (derived-mode-p 'org-mode)
        (save-excursion
          (let ((end (save-excursion (outline-next-heading) (point))))
            (if (re-search-forward "- Note taken" end t)
                "*" " ")))
      " "))

  :custom
  (org-agenda-files '("~/SynologyDrive/Sylvain/Org/agenda.org"
                      "~/SynologyDrive/Sylvain/Org/someday.org"
                      "~/SynologyDrive/Sylvain/Org/specialdays.org"))

  ;; Week start on sunday
  (org-agenda-start-on-weekday 0)

  ;; No recursive todo in agenda
  (org-agenda-todo-list-sublevels nil)

  ;; Ignore scheduled and deadline in TODO lists
  (org-agenda-todo-ignore-scheduled 'all)
  (org-agenda-todo-ignore-deadlines 'all)

  (org-agenda-skip-deadline-if-done t)

  ;; Remove tags in agenda
  (org-agenda-remove-tags t)

  (org-agenda-window-setup 'other-window)

  ;; Compact the block agenda view
  (org-agenda-compact-blocks t)

  ;; Custom agenda view
  (agenda-custom-commands
   '(("b" "Thesis Work" tags-todo "boss")
     ("t" "All TODO" alltodo "")))

  ;; Icons in agenda
  (org-agenda-category-icon-alist
   '(("Emacs" "/usr/local/share/icons/hicolor/16x16/apps/emacs.png" nil nil :ascent center)
     ("Books\\|Magazines" "~/.emacs.d/icons/book.png" nil nil :ascent center)
     ("Anniv" "~/.emacs.d/icons/birthday.png" nil nil :ascent center)
     ("Fête" "~/.emacs.d/icons/party-hat.png" nil nil :ascent center)
     ("Férié" "~/.emacs.d/icons/flip_flops.png" nil nil :ascent center)
     ("Schlink!" "~/.emacs.d/icons/euro.png" nil nil :ascent center)
     ("Argent" "~/.emacs.d/icons/euro.png" nil nil :ascent center)
     ("Santé" "~/.emacs.d/icons/syringe.png" nil nil :ascent center)
     ("Download" "~/.emacs.d/icons/download.png" nil nil :ascent center)
     ("Series" "~/.emacs.d/icons/tv.png" nil nil :ascent center)
     ("Movie" "~/.emacs.d/icons/film.png" nil nil :ascent center)
     ("Bébé" "~/.emacs.d/icons/baby.png" nil nil :ascent center)
     ("Football" "~/.emacs.d/icons/football.svg" nil nil :ascent center)
     ("Cours" "~/.emacs.d/icons/hat.png" nil nil :ascent center)
     ("" '(space . (:height (16) :width (16))))))

  :bind*
  ("C-c c" . org-capture)
  ("C-c l" . org-store-link)
  ("C-c L" . org-insert-link-global)
  ("C-c a" . org-agenda)

  :config
  ;; Annoted todo are stared
  (add-to-list 'org-agenda-prefix-format
               '(todo . " %(annotedp)%i %-12:c"))
  (add-to-list 'org-agenda-prefix-format
               '(agenda . " %(annotedp)%i %-12:c%?-12t% s"))

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
  (define-key org-agenda-mode-map "X" #'org-agenda-mark-done-and-add-followup)

  (define-key org-agenda-mode-map [(control return)] #'org-agenda-add-report))

(use-package calendar
  :straight nil
  :custom
  (calendar-time-zone-style 'numeric)
  (calendar-date-style 'european)
  (calendar-mark-holidays-flag t))

(use-package holidays
  :straight nil
  :demand :after org-agenda
  :custom
  (calendar-holidays
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
     ;;        (holiday-easter-etc -47 "Mardi gras")
     (holiday-easter-etc 50 "Lundi de Pentecôte"))))

(use-package appt
  :straight nil
  :preface
  (defun appt-display (mins current-time msgs)
    (seq-mapn
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
        :app-icon (check-filepath "~/.emacs.d/icons/appointment-soon.png")
        :sound-file (check-filepath "/usr/share/sounds/ubuntu/stereo/phone-incoming-call.ogg"
                                    "/usr/share/sounds/gnome/default/alerts/sonar.ogg"
                                    "/usr/share/sounds/freedesktop/stereo/bell.oga")))
     (if (listp mins) mins (list mins))
     (if (listp msgs) msgs (list msgs))))

  :custom
  ;; Warning with appt and notify
  (appt-message-warning-time 20)
  (appt-display-interval 3)
  (appt-display-mode-line t)     ;; show in the modeline
  (appt-display-format 'window) ;; use our func
  (appt-disp-window-function 'appt-display)
  (appt-delete-window-function 'ignore)

  :config
  (appt-activate 1) ;; active appt (appointment notification)
  (display-time)    ;; time display is required for this...
  )

(use-package parse-time
  :straight nil
  :config
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
                  ("samedi" . 6)))))

(provide 'init-org)
