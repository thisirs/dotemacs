(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/org-mode/lisp"))
(require 'org)

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

;; load templates from personnal location
(setq org-capture-templates
      (load-file-to-list "~/Dropbox/Org/org-capture-templates.el"))

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

;; (setq parse-time-months '(("jan" . 1) ("fev" . 2) ("mar" . 3)
;;                             ("avr" . 4) ("mai" . 5) ("juin" . 6)
;;                             ("juil" . 7) ("aout" . 8) ("sep" . 9)
;;                             ("oct" . 10) ("nov" . 11) ("dec" . 12)
;;                             ("janvier" . 1) ("février" . 2)
;;                             ("mars" . 3) ("avril" . 4) ("juin" . 6)
;;                             ("juillet" . 7) ("aout" . 8)
;;                             ("septembre" . 9) ("octobre" . 10)
;;                             ("novembre" . 11) ("décembre" . 12)))
;; (setq parse-time-weekdays '(("sun" . 0) ("mon" . 1) ("tue" . 2)
;;                               ("wed" . 3) ("thu" . 4) ("fri" . 5)
;;                               ("sat" . 6) ("sunday" . 0) ("monday" . 1)
;;                               ("tuesday" . 2) ("wednesday" . 3)
;;                               ("thursday" . 4) ("friday" . 5)
;;                               ("saturday" . 6)))

(provide 'init-org)
