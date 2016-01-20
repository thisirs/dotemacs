(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/org-mode/lisp"))
(require 'org)

(add-to-list 'load-path "~/.emacs.d/site-lisp/org-mode/contrib/lisp")
(require 'org-drill)

;; Workaround to use yasnippet in org-mode
(defun yas-org-very-safe-expand ()
  (let ((yas-fallback-behavior 'return-nil))
    (yas-expand)))

(add-to-list 'org-tab-first-hook 'yas-org-very-safe-expand)

(setq org-todo-keywords
      '("TODO" "|" "CANCELLED" "DONE"))

;; No recursive todo in agenda
(setq org-agenda-todo-list-sublevels nil)

;; Ignore scheduled and deadline
(setq org-agenda-todo-ignore-scheduled t)
(setq org-agenda-todo-ignore-deadlines t)

;; Allow property inheritance for org-context agenda in my scripts
;; directory
(setq org-use-property-inheritance t)

;; Set org id locations
(setq org-id-locations-file "~/CloudStation/Sylvain/Org/.org-id-locations")

;; Remove tags in agenda
(setq org-agenda-remove-tags t)

;; Special navigation in org mode
(setq org-special-ctrl-a/e t)

;; Fontify src blocks
(setq org-src-fontify-natively t)

(setq org-src-tab-acts-natively t)

;; Restore windows configuration
;; (setq org-agenda-restore-windows-after-quit t)
(setq org-agenda-window-setup 'other-window)

;; Shorter description
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

;; Function to replace the description in a link
(defun org-replace-name-in-link (link &optional here)
  (setq here (or here "here"))
  (if (string-match "\\[\\(\\[.*?\\]\\)\\(\\[.*?\\]\\)?\\]" link)
      (replace-match (concat "[\\1[" here "]]") nil nil link)
    link))


(require 'init-org-capture-helpers)

;; Load templates from personal location
(setq org-capture-templates
      (load-file-to-list "~/CloudStation/Sylvain/emacs/org-capture-templates.el"))


(define-key global-map "\C-cc" #'org-capture)

;; Custom agenda view
(setq org-agenda-custom-commands
      '(("b" "Thesis Work" tags-todo "boss")
        ("t" "All TODO" alltodo "")))

;; Icons in agenda
(setq org-agenda-category-icon-alist
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

;; Annoted todo are stared
(eval-after-load "org-agenda"
  '(add-to-list 'org-agenda-prefix-format
                '(todo . " %(annotedp)%i %-12:c")))

(defun annotedp ()
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (let ((end (save-excursion (outline-next-heading) (point))))
        (if (re-search-forward "- Note taken" end t)
            "*" " ")))))

;; Logging
(setq org-log-done 'time)

(setq org-agenda-skip-deadline-if-done t)

(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c L") #'org-insert-link-global)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c b") #'org-iswitchb)

;; Support for links in twittering-mode
(defun org-twittering-store-link ()
  (when (eq major-mode 'twittering-mode)
    (let ((uri (or (get-text-property (point) 'uri)
                   (if (get-text-property (point) 'field)
                       (let ((id (or (get-text-property (point) 'retweeted-id)
                                     (get-text-property (point) 'id)))
                             (username (get-text-property (point) 'username)))
                         (twittering-get-status-url username id))
                     nil))))
      (and (stringp uri)
           (org-store-link-props :type "http" :link uri)
           t))))

(add-to-list 'org-store-link-functions 'org-twittering-store-link)

(mapc (lambda (file)
        (if (file-exists-p file)
            (add-to-list 'org-agenda-files file)))
      '("~/CloudStation/Sylvain/Org/agenda.org"
        "~/CloudStation/Sylvain/Org/someday.org"
        "~/CloudStation/Sylvain/Org/specialdays.org"
        "~/CloudStation/Sylvain/Org/books.org"
        "~/CloudStation/Sylvain/Org/series.org"
        ))

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
(defun mardi-gras ()
  (let ((abs-easter (abs-easter)))
    (and (calendar-date-equal
          date
          (calendar-gregorian-from-absolute (+ abs-easter -47))))))

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

;; Don't wait for agenda to be opened to update appt
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
        ;;        (holiday-easter-etc -47 "Mardi gras")
        (holiday-easter-etc 50 "Lundi de Pentecôte")))


;; (holiday-float 6 0 3 "Fête des pères")
;; (if (equal (holiday-float 5 0 -1 "") (holiday-easter-etc 49 ""))
;;     (holiday-float 6 0 1 "Fête des mères!!")
;;   (holiday-float 5 0 -1 "Fête des mères!!"))

(setq calendar-date-style 'european
      calendar-holidays french-holiday
      calendar-mark-holidays-flag t)

;; Warning with appt and notify
(setq
 appt-message-warning-time 15 ;; warn 15 min in advance
 appt-display-interval 3
 appt-display-mode-line t     ;; show in the modeline
 appt-display-format 'window) ;; use our func

(appt-activate 1)              ;; active appt (appointment notification)
(display-time)                 ;; time display is required for this...

;; Update appt each time agenda is opened, force a refresh to cancel
;; Deleted appt
(add-hook 'org-finalize-agenda-hook
          (lambda () (org-agenda-to-appt 1)))

;; Our little façade-function for djcb-popup
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
(setq appt-delete-window-function 'ignore)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (latex . t) ; this is the entry to activate LaTeX
   (sh . t)
   (matlab . t)
   (ruby . t)
   (python . t)))

(setq org-babel-sh-command "bash")

;; Bib citations in org files
(defun org-mode-reftex-setup ()
  (reftex-mode t)
  (when (and (buffer-file-name)
             (file-exists-p (buffer-file-name)))
    (reftex-parse-all)
    (reftex-set-cite-format "[[note::%l][%l]]")
    (define-key org-mode-map (kbd "C-c )") #'reftex-citation)))

(add-hook 'org-mode-hook 'org-mode-reftex-setup)

;; Open pdf files with acroread or evince (fuck gv)
(and (executable-find "acroread")
     (push (cons "pdf" "acroread %s") org-file-apps))
(and (executable-find "evince")
     (push (cons "pdf" "evince %s") org-file-apps))

;; Open ods files with libreoffice
(add-to-list 'org-file-apps '("\\.od[st]\\'" . "soffice %s"))
(add-to-list 'org-file-apps '("\\.docx?\\'" . "soffice %s"))
(add-to-list 'org-file-apps '("\\.xlsx?\\'" . "soffice %s"))
(add-to-list 'org-file-apps '("\\.mp4\\'" . "vlc %s"))
(add-to-list 'org-file-apps '("\\.avi\\'" . "vlc %s"))
(add-to-list 'org-file-apps '("\\.flv\\'" . "vlc %s"))

;; Open directories with dired
(add-to-list 'org-file-apps '(directory . emacs))

;; Bigger latex fragment
(plist-put org-format-latex-options :scale 1.5)

;; (defun jump-to-org-agenda ()
;;   (interactive)
;;   (let ((buf (or (get-buffer "*Org Agenda*")
;;                  (get-buffer "*Org Agenda(a)*")))
;;         wind)
;;     (if buf
;;         (if (setq wind (get-buffer-window buf))
;;             (when (called-interactively-p 'any)
;;               (select-window wind)
;;               (org-fit-window-to-buffer))
;;           (if (called-interactively-p 'any)
;;               (progn
;;                 (select-window (display-buffer buf t t))
;;                 (org-fit-window-to-buffer))
;;             (with-selected-window (display-buffer buf)
;;               (org-fit-window-to-buffer))))
;;       (call-interactively 'org-agenda-list))))

;; (run-with-idle-timer 900 t 'jump-to-org-agenda)

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
    (org-align-all-tags)
    (org-update-all-dblocks)))

(add-hook 'before-save-hook 'clean-org-buffer)

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
                 (regexp-quote (org-make-link-string (format "file:%s" name) name))) nil t)
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
                      (org-make-link-string (format "file:%s" name) name)
                      (format-time-string
                       "[%Y-%m-%d %a %H:%M:%S]"
                       (nth 6 (file-attributes (expand-file-name name base-dir))))))
      (org-id-get nil 'create))))

;; Compact the block agenda view
(setq org-agenda-compact-blocks t)

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

(add-hook 'org-after-todo-statistics-hook 'org-update-project-cookies)

(defvar org-other-files nil
  "List of org files other than agenda files destined to be
refile targets.")

;; First open someday.org and look for org files to add to
;; org-other-files in "Projects" headline.
(find-file "~/CloudStation/Sylvain/Org/someday.org")

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

(setq org-refile-targets
      `((,org-agenda-files . (:level . 1))
        (,org-other-files . (:level . 0))))

(setq org-refile-use-outline-path 'full-file-path)

(setq org-outline-path-complete-in-steps nil)

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

(define-key org-agenda-mode-map [(control return)] #'org-agenda-add-report)

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
      'org-latex-format-headline-checkbox-function)

;; Automatic expire mechanism
(when (file-exists-p "~/CloudStation/Sylvain/emacs/site-lisp/org-expiry")

  (add-to-list 'load-path "~/CloudStation/Sylvain/emacs/site-lisp/org-expiry")
  (require 'org-expiry)

  (setq org-expiry-handler-function 'org-archive-subtree)
  (setq org-expiry-confirm-flag nil)

  (defun org-auto-archive ()
    (message "Auto-archiving...")
    (mapcar
     (lambda (file)
       (let ((buf (find-buffer-visiting file)))
         (when buf
           (with-current-buffer buf
             (org-expiry-process-entries nil nil t)
             ;; save buffer as it is not save if org-auto-archive is
             ;; called from kill-emacs-hook
             (save-buffer)))))
     '("~/CloudStation/Sylvain/Org/someday.org"
       "~/CloudStation/Sylvain/Org/agenda.org"))
    (message "Auto-archiving...done"))

  (on-zbook
   (add-hook 'kill-emacs-hook 'org-auto-archive)))

;; electric-indent-mode doesn't play well with org
(with-emacs-version>= "24.1"
  (add-hook 'org-mode-hook
            (lambda ()
              (set (make-local-variable 'electric-indent-functions)
                   (list (lambda (arg) 'no-indent))))))

(define-key org-mode-map (kbd "C-c SPC")
  (lambda ()
    (interactive)
    (require 'org-table)
    (if (org-table-check-inside-data-field 'noerror)
        (org-table-blank-field)
      (call-interactively 'ace-jump-mode))))

(require 'ox-koma-letter)

;; email is specified is lco file
(setq org-koma-letter-email nil)

;; no backaddress by default
(setq org-koma-letter-use-backaddress nil)

;; no foldmarks by default
(setq org-koma-letter-use-foldmarks nil)

;; Don't warn when a link run `org-agenda-from-file'
(setq org-confirm-elisp-link-not-regexp
      "\\`(org-context-agenda-from \".*\" \"[a-zA-Z]+\")\\'")

(provide 'init-org)
