(add-to-list 'load-path (expand-file-name "~/.emacs.d"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/org-mode/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/anything-config"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/anything-config/extensions"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/auto-install"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/apel-10.7"))
;; (load "elscreen" "ElScreen" t)

;; Désactivation des boites de dialogue
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(tool-bar-mode -1)

(require 'auto-install)

(require 'instant)

(require 'notify-send)

(require 'letter)

;; abandonne le minibuffer quand on le quitte
(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

;; ouvre un buffer en sudo via tramp
(defun sudo-edit (&optional arg)
  (interactive "p")
  (let ((tramp-prefix "/sudo::"))
    (if (and
          (= arg 1)
          (buffer-file-name)
          (not (string-match "^/sudo::" buffer-file-name)))
      (find-alternate-file (concat "/sudo::" buffer-file-name))
      (find-file (concat "/sudo::"
                   (ido-read-file-name "File: "))))))

(defun find-temp-file (extension)
  "quick find file in /tmp/"
  (interactive "sExtension: ")
  (cond
    ((equal (length extension) 0) (find-file (make-temp-file "foo")))
    ((memq ?. (string-to-list extension))
      (find-file (concat "/tmp/" extension)))
    (t (find-file (concat (make-temp-file "foo") "." extension)))))

(global-set-key (kbd "C-x C-t") 'find-temp-file)

;; shortcut for reverting a buffer
(global-set-key (kbd "C-x C-r") 'revert-buffer)

(global-set-key (kbd "<C-kp-6>") 'enlarge-window-horizontally)
(global-set-key (kbd "<C-kp-4>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-kp-2>") 'enlarge-window)
(global-set-key (kbd "<C-kp-8>") 'shrink-window)

;; don't suspend
(global-unset-key "\C-z")

(require 'linkd)


;;; anything
(require 'xml)
(require 'anything-startup)
(setq anything-command-map-prefix-key "C-x C-a")

(setq anything-su-or-sudo "sudo")

(setq anything-c-locate-command "locate -e -b -i -r \"%s\"")

(defun my-anything ()
  (interactive)
  (anything-other-buffer
    '(anything-c-source-ffap-line
       anything-c-source-ffap-guesser
       anything-c-source-buffers+
       anything-c-source-recentf
       anything-c-source-bookmarks
       anything-c-source-file-cache
       anything-c-source-files-in-current-dir+
       anything-c-source-locate-thiskey
       anything-c-source-locate) "*anything for files*"))

(define-key anything-command-map (kbd "f") 'my-anything)

(defun update-locate-database ()
  "Update locate databases"
  (interactive)
  (set-process-sentinel (start-process-shell-command "updatedb process" nil
                          "updatedb -l 0"
                          "-U /media/THISKEY/"
                          "--add-prunepaths \"/media/THISKEY/.Trash-1000 /media/THISKEY/.Trash-1001\""
                          "-o $HOME/.locate.db")
    (lambda (process event)
      (if (string-match "^finished" event)
        (message "Locate database updated!")
        (message "Updating locate database failed!")))))

(defun anything-c-locate-thiskey-init ()
  "Initialize async locate process for `anything-c-source-locate'."
  (start-process-shell-command "locate-thiskey-process" nil
    (format (concat "locate -e -b -d " (expand-file-name "~/.locate.db") " -i -r \"%s\"")
      anything-pattern)))

(defvar anything-c-source-locate-thiskey
  '((name . "Locate in THISKEY")
     (candidates . anything-c-locate-thiskey-init)
     (type . file)
     (requires-pattern . 3)
     (delayed))
  "Find files matching the current input pattern with locate.")

(defun anything-translate ()
  (interactive)
  (anything-other-buffer
    '(anything-c-source-google-translate) "*anything translate*"))


(defvar anything-c-source-google-translate
  '((name . "Google translate")
     (dummy)
     (delayed)
     (filtered-candidate-transformer . (lambda (candidates source)
                                         (anything-c-google-translate)))))


(defun anything-c-google-translate ()
  (let ((request
          (concat
            "http://ajax.googleapis.com/ajax/services/language/translate?v=1.0&q="
            (url-hexify-string anything-pattern)
            "&langpair=fr%7Cen")))
    (with-temp-buffer
      (call-process "curl" nil '(t nil) nil request)
      (goto-char (point-min))
      (if (re-search-forward "translatedText\":\"\\([^\"]+\\)\"" nil t)
        (list (match-string 1))
        nil))))

(define-key anything-command-map (kbd "t") 'anything-translate)

;; boss key!

(defvar boss-window-configuration nil "Window configuration to switch to when the boss comes!")
(defvar my-window-configuration nil "Window configuration to switch back to!")

(defun define-current-window-configuration nil
  "Define current window configuration as boss's one"
  (interactive)
  (setq boss-window-configuration (current-window-configuration))
  (message "Boss window configuration set to current!"))

(defun boss nil
  "Switch to boss windows configuration"
  (interactive)
  (if (equal (current-window-configuration) boss-window-configuration)
    (set-window-configuration my-window-configuration)
    (setq my-window-configuration (current-window-configuration))
    (set-window-configuration boss-window-configuration)))

(global-set-key (kbd "²") 'boss)
(global-set-key (kbd "C-²") 'define-current-window-configuration)


;; winner-mode
(winner-mode 1)
(setq winner-boring-buffers
  '("*Completions*" "*anything for files*" "*anything find-file*"))


;; M-RET to keep writing comments, clashes with auctex mode
;; (global-set-key (kbd "M-RET") comment-line-break-function)

;; magit
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/magit"))
(require 'magit)
(global-set-key "\C-ci" 'magit-status)

;; suit les liens vers système de contrôles de versions
(setq vc-follow-symlinks t)

(defalias 'yes-or-no-p 'y-or-n-p)

;; (setq confirm-kill-emacs
;;   (lambda (e)
;;     (y-or-n-p-with-timeout
;;       "Really exit Emacs (automatically exits in 5 secs)? " 5 t)))

;; Laisser le curseur en place lors d'un défilement par pages. Par
;; défaut, Emacs place le curseur en début ou fin d'écran selon le
;; sens du défilement.
(setq scroll-preserve-screen-position t)

;; sélection avec SHIFT
;;(custom-set-variables '(pc-selection-mode t nil (pc-select)))

(load-library "paren")
(show-paren-mode 1)

(defun rename-current-file-or-buffer ()
  (interactive)
  (if (not (buffer-file-name))
    (call-interactively 'rename-buffer)
    (let ((file (buffer-file-name)))
      (with-temp-buffer
        (set-buffer (dired-noselect file))
        (dired-do-rename)
        (kill-buffer nil))))
  nil)


;;; matlab mode
(autoload 'matlab-mode "matlab" "Enter MATLAB mode." t)
(setq auto-mode-alist (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))
(autoload 'matlab-shell "matlab" "Interactive MATLAB mode." t)
(add-hook 'matlab-mode-hook
  (lambda ()
    (setq fill-column 76)                       ; where auto-fill should wrap
    (turn-on-auto-fill)
    (setq matlab-functions-have-end t)
    (setq matlab-indent-function-body t)
    (setq matlab-indent-function t)))

;; history navigation
(eval-after-load "comint"
  '(progn
     (define-key comint-mode-map [(control ?p)] 'comint-previous-input)
     (define-key comint-mode-map [(control ?n)] 'comint-next-input)))

(require 'ibuffer)

;; don't show empty groups
(setq ibuffer-show-empty-filter-groups nil)

(require 'cl) ; needed on some install to be able to use reduce...
(defun find-projects (dir)
  (let ((dir (if (string= (substring dir -1 nil) "/")
               dir (concat dir "/"))))
    (if (not (file-directory-p dir))
      nil
      (reduce (lambda (list prj)
                (let ((prj-dir (concat dir prj)))
                  (cond
                    ((null prj) nil)
                    ((and (file-exists-p (concat prj-dir "/.git"))
                       (not (file-exists-p (concat prj-dir "/.hidden"))))
                      (cons `(,prj . ,prj-dir) list))
                    ((file-directory-p prj-dir)
                      (append (find-projects prj-dir) list))
                    (t list))))
        (cddr (directory-files dir))
        :initial-value nil))))

(defun make-ibuffer-projects-list (prefix dir)
  (reduce (lambda (list prj)
            (cons
              `(,(concat prefix (car prj)) (filename . ,(cdr prj)))
              list))
    (find-projects dir)
    :initial-value nil))

(setq ibuffer-saved-filter-groups
  `(("default"
      ,@(make-ibuffer-projects-list "Project: "
          (concat (getenv "HOME") "/dotemacs"))
      ("ICIP article"
        (or
          (filename . "/media/THISKEY/Documents/article_ICIP_2010/")
          (filename . "/media/THISKEY/Documents/IMG_PROC_revue/")))
      ("TP IMAGE 2010/2011"
        (or
          (filename . "/media/THISKEY/enseignements/2010-2011/TP_IMAGE/")))
      ("Org"
        (mode . org-mode))
      ("TeX/LaTeX"
        (or
          (mode . latex-mode)
          (name . "^\\.tex$")))
      ("Mail"
        (or
          (mode . message-mode)
          (mode . mail-mode)
          ))
      ("Dired"
        (mode . dired-mode)
        )
      ("THISKEY's programming"
        (filename . "/media/THISKEY/programming/"))
      ("Programming"
        (or
          (mode . c-mode)
          (mode . perl-mode)
          (mode . python-mode)
          (mode . emacs-lisp-mode)
          (mode . ruby-mode)
          (mode . sh-mode)
          ))
      ("crap" (or
                (name . "^\\*trace")
                (name . "^\\*completions")
                (name . "^\\*Quail")
                (name . "^\\*magit")
                (name . "^\\*Backtrace\\*$")
                (name . "^\\*compilation\\*$")
                (name . "^\\*scratch\\*$")
                (name . "^\\*Messages\\*$")))
      ("ERC" (mode . erc-mode)))))

(add-hook 'ibuffer-mode-hook
  (lambda ()
    (ibuffer-switch-to-saved-filter-groups "default")))

(global-set-key (kbd "C-x C-b") 'ibuffer)

;;; indenter automatiquement le code collé :
(defadvice yank (after indent-region activate)
  (if (member major-mode '(ruby-mode emacs-lisp-mode scheme-mode
                            lisp-mode c-mode c++-mode objc-mode
                            latex-mode plain-tex-mode
                            python-mode))
    (indent-region (region-beginning) (region-end) nil)))

(defadvice yank-pop (after indent-region activate)
  (if (member major-mode '(ruby-mode emacs-lisp-mode scheme-mode
                            lisp-mode c-mode c++-mode objc-mode
                            latex-mode plain-tex-mode
                            python-mode))
    (indent-region (region-beginning) (region-end) nil)))



;; kill-ring-save (M-w) copie la ligne si aucune region active,
;; copie le groupe si au dessus d'un délimiteur
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
    (cond
      (mark-active (list (region-beginning) (region-end)))
      ((looking-at "[])}]") (message "group copied!")
        (list (1+ (point)) (save-excursion (forward-char) (backward-list))))
      ((looking-at "[[({]") (message "group copied!")
        (list (point) (save-excursion (forward-list))))
      (t (message "line copied!") (list (line-beginning-position)
                                    (line-beginning-position 2)))))
  (setq last-buffer-we-cut-from (or buffer-file-name (buffer-name))))


;; kill-region (C-w) coupe la ligne courante si aucune région active
;; coupe le groupe si au dessus d'un délimiteur
(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
    (cond
      (mark-active (list (region-beginning) (region-end)))
      ((looking-at "[])}]")
        (list (1+ (point)) (save-excursion (forward-char) (backward-list))))
      ((looking-at "[[({]")
        (list (point) (save-excursion (forward-list))))
      (t (list (line-beginning-position)
           (line-beginning-position 2)))))
  (setq last-buffer-we-cut-from (expand-file-name buffer-file-name)))


;;; la commande kill supprime automatiquement les espaces
;;; d'indentations si besoin
(defadvice kill-line (before check-position activate)
  (if (member major-mode '(emacs-lisp-mode scheme-mode lisp-mode
                            c-mode c++-mode objc-mode
                            latex-mode plain-tex-mode
                            ruby-mode python-mode))
    (if (and (eolp) (not (bolp)))
      (progn (forward-char 1)
        (just-one-space 0)
        (backward-char 1)))))

(defun my-beginning-of-line ()
  (interactive)
  (if (bolp)
    (beginning-of-line-text)
    (beginning-of-line)))

(global-set-key (kbd "C-a") 'my-beginning-of-line)


(defun my-find-thing-at-point ()
  "Find variable, function or file at point."
  (interactive)
  (cond ((not (eq (variable-at-point) 0))
          (call-interactively 'describe-variable))
    ((function-called-at-point)
      (call-interactively 'describe-function))
    (t (find-file-at-point))))

(global-set-key (kbd "C-x C-p") 'my-find-thing-at-point)

;; quick bind to f1 to try out
(defmacro bind-to-f1 (&rest prog)
  `(global-set-key [f1]
     (lambda ()
       (interactive)
       ,@prog)))


;; Put the cursor in an intelligent place when searching
(add-hook 'isearch-mode-end-hook 'custom-goto-match-beginning)
(defun custom-goto-match-beginning ()
  "Use with isearch hook to end search at first char of match"
  (and isearch-forward
    (not mark-active)
    (not isearch-mode-end-hook-quit)
    (goto-char isearch-other-end)))

;; occur mode
(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
               (regexp-quote isearch-string))))))

;; staying in isearch mode when typing M-< M-> C-l
(defun isearch-beginning-of-buffer ()
  "Move isearch point to the beginning of the buffer."
  (interactive)
  (goto-char (point-min))
  (isearch-repeat-forward))

(define-key isearch-mode-map "\M-<" 'isearch-beginning-of-buffer)

(defun isearch-end-of-buffer ()
  "Move isearch point to the end of the buffer."
  (interactive)
  (goto-char (point-max))
  (isearch-repeat-backward))

(define-key isearch-mode-map "\M->" 'isearch-end-of-buffer)

(define-key isearch-mode-map "\C-l" 'recenter-top-bottom)

;; echo keystrokes quickly
(setq echo-keystrokes 0.1)

;; textmate-next-line from textmate.el - github.com/defunkt/textmate.el
(defun textmate-next-line ()
  "Go to next line and indent wherever you are in a line"
  (interactive)
  (end-of-line)
  (newline-and-indent))
(global-set-key [C-return] 'textmate-next-line)

;; FIXME: make it for all coding modes
;; auto-fill-mode uniquement pour les commentaires
;;(set (make-local-variable 'comment-auto-fill-only-comments) t)
;;(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; efface tous les espaces et sauts de ligne avec un seul backspace
(setq backward-delete-char-untabify-method (quote all))

;; move between windows with meta-arrows
;; (windmove-default-keybindings 'meta)
(global-set-key (kbd "s-b") 'windmove-left)
(global-set-key (kbd "s-f") 'windmove-right)
(global-set-key (kbd "s-p") 'windmove-up)
(global-set-key (kbd "s-n") 'windmove-down)

;; Mettre un titre aux fenêtres
(setq frame-title-format '(buffer-file-name "Emacs: %b (%f)" "Emacs: %b"))

;; Non au défilement qui accélère
(setq mouse-wheel-progressive-speed nil)

;; sélection de tout le buffer
(global-set-key "\C-c\C-a" 'mark-whole-buffer)

;; pas de file<2> quand 2 buffers ont le même nom
(require 'uniquify)
(setq
  uniquify-buffer-name-style 'post-forward-angle-brackets
  uniquify-after-kill-buffer-p t)

;; put something different in the scratch buffer
(setq initial-scratch-message
  ";; scratch buffer created -- happy hacking\n\n")

;; override the default function....
(defun emacs-session-filename (SESSION-ID)
  (concat "~/.emacs.d/cache/session." SESSION-ID))

;; backups
(setq make-backup-files t ;; do make backups
  ;;  backup-by-copying t     ;; and copy them here
  backup-directory-alist '((".*" . "~/.emacs.d/emacs.backups"))
  version-control t
  kept-new-versions 2
  kept-old-versions 5
  delete-old-versions t)

(setq auto-save-list-file-prefix
  "~/.emacs.d/cache/auto-save-list/.saves-")
(setq auto-save-file-name-transforms
  `((".*" ,temporary-file-directory t)))

;; ouverture rapide avec la touche windows
(global-set-key (kbd "s-s s") ;; scratch
  (lambda () (interactive) (switch-to-buffer "*scratch*")))
(global-set-key (kbd "s-s e") ;; .emacs
  (lambda () (interactive) (find-file "~/.emacs")))

(global-set-key (kbd "C-x à") 'delete-other-windows)
(global-set-key (kbd "C-x C-à") 'delete-other-windows)

;; split screen and switch to it!
(global-set-key (kbd "C-x 3")
  (lambda nil
    (interactive)
    (split-window-horizontally)
    (other-window 1)))

(global-set-key (kbd "C-x 2")
  (lambda nil
    (interactive)
    (split-window-vertically)
    (other-window 1)))


;; save a list of open files in ~/.emacs.desktop
;; save the desktop file automatically if it already exists
(setq desktop-save 'if-exists)
(desktop-save-mode 1)

;; save a bunch of variables to the desktop file
;; for lists specify the len of the maximal saved data also
(setq desktop-globals-to-save
  (append '((extended-command-history . 30)
             (file-name-history        . 100)
             (grep-history             . 30)
             (compile-history          . 30)
             (minibuffer-history       . 50)
             (query-replace-history    . 60)
             (read-expression-history  . 60)
             (regexp-history           . 60)
             (regexp-search-ring       . 20)
             (search-ring              . 20)
             (shell-command-history    . 50)
             tags-file-name
             register-alist)))

(setq desktop-files-not-to-save "\\(^/[^/:]*:\\|(ftp)$\\)\\|\\(^/tmp/\\)")
(setq desktop-buffers-not-to-save
  (concat "\\(" "^nn\\.a[0-9]+\\|\\.log\\|(ftp)"
    "\\)$"))

(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)

;; don't let Customize mess with my .emacs
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;;; color theme
(add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0")
(require 'color-theme)

(defun color-theme-railscasts ()
  (interactive)
  (color-theme-install
    '(color-theme-railscasts
       ((background-color . "#232323")
         (background-mode . dark)
         (cursor-color . "#5A647E")
         (foreground-color . "#E6E1DC"))
       (default ((t (nil))))
       (bold ((t (:bold t))))
       (bold-italic ((t (:italic t :bold t))))
       (fringe ((t (:background "#232323"))))
       (font-lock-builtin-face ((t (:foreground "#D0D0FF"))))
       (font-lock-comment-face ((t (:foreground "#BC9458" :italic t))))
       (font-lock-constant-face ((t (:foreground "#6D9CBE"))))
       (font-lock-doc-string-face ((t (:foreground "#A5C261"))))
       (font-lock-function-name-face ((t (:foreground "#FFC66D"))))
       (font-lock-keyword-face ((t (:foreground "#CC7833"))))
       (font-lock-preprocessor-face ((t (:foreground "#CC7833"))))
       (font-lock-reference-face ((t (:foreground "LightSteelBlue"))))
       (font-lock-string-face ((t (:foreground "#A5C261"))))
       (font-lock-type-face ((t (:foreground "white"))))
       (font-lock-variable-name-face ((t (:foreground "LightSteelBlue"))))
       (font-lock-warning-face ((t (:foreground "Pink"))))
       (paren-face-match-light ((t (:foreground "#FFC66D" :background "#555577"))))
       (highlight ((t (:background "darkolivegreen"))))
       (italic ((t (:italic t))))
       (modeline ((t (:background "#A5BAF1" :foreground "black"))))
       (modeline-buffer-id ((t (:background "#A5BAF1" :foreground
                                 "black"))))
       (modeline-mousable ((t (:background "#A5BAF1" :foreground
                                "black"))))
       (modeline-mousable-minor-mode ((t (:background
                                           "#A5BAF1" :foreground "black"))))
       (region ((t (:background "#555577"))))
       (primary-selection ((t (:background "#555577"))))
       (isearch ((t (:background "#555555"))))
       (zmacs-region ((t (:background "#555577"))))
       (secondary-selection ((t (:background "darkslateblue"))))
       (flymake-errline ((t (:background "LightSalmon" :foreground
                              "black"))))
       (flymake-warnline ((t (:background "LightSteelBlue" :foreground
                               "black"))))
       (underline ((t (:underline t))))
       (minibuffer-prompt ((t (:bold t :foreground "#FF6600"))))
       ;; two org-mode faces
       (org-document-info-keyword ((t (:foreground "#BC9458" :bold t))))
       (org-document-title ((t (:foreground "#BC9458" :bold t))))
       )))

(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-railscasts)))

;;; org-mode
(require 'org-install)

(setq org-todo-keywords
  '("TODO" "|" "CANCELLED" "DONE"))

(setq org-capture-templates
  '(("t" "Todo" entry
      (file+headline "~/Dropbox/Org/someday.org" "Tâches")
      "* TODO %?\n  OPENED: %U")
     ("p" "Programming" entry
       (file+headline "~/Dropbox/Org/someday.org" "Programming")
       "* TODO %?\n  OPENED: %U")
     ("e" "Event" entry
       (file+headline "~/Dropbox/Org/agenda.org" "Divers")
       "* EVENT %?")
     ("qu" "Quote" entry
       (file+headline "~/Dropbox/Org/quotes.org" "")
       "* %^{description}%?\n  OPENED: %U")
     ("a" "Anniv" entry
       (file+headline "~/Dropbox/Org/birthday.org" "")
       "* %^{Birthday}t Anniversaire de %^{prompt}!\n")
     ("f" "Phone calls")
     ("fr" "Received phone calls" entry
       (file+headline "~/Dropbox/Org/phonecalls.org" "Received")
       "* Received phone call:\n  at %U\n  from %?")
     ("fm" "Made phone calls" entry
       (file+headline "~/Dropbox/Org/phonecalls.org" "Made")
       "* Made phone call:\n  at %U\n  to %?")
     ("b" "Livres empruntés")
     ("bu" "Bibliothèque Universitaire" entry
       (file+headline "~/Dropbox/Org/books.org" "Empruntés")
       "* BORROWED %?\n  BU Universitaire\n  BORROWED: %u\n  DEADLINE: %(add-days 28 4)")
     ("bl" "Bibliothèque du labo" entry
       (file+headline "~/Dropbox/Org/books.org" "Empruntés")
       "* BORROWED %?\n  BU Labo\n  BORROWED: %u\n  DEADLINE:  %(add-days 365 4)")))

(defun add-days (days &optional deadline)
  "Construit la date de retour avec une deadline"
  (let ((time (format-time-string
		(car org-time-stamp-formats)
		(time-add (current-time) (seconds-to-time (* 24 3600 days))))))
    (if (integerp deadline)
      (replace-regexp-in-string ">"
	(concat " -" (format "%d" deadline) "d>")
	time)
      time)))

(define-key global-map "\C-cc" 'org-capture)

;; Ne mettre qu'une seule étoile devant les titres
;; FIXME marche pas quand on change le color-theme
;; (setq org-hide-leading-stars t)

;; logging
(setq org-log-done 'time)

(setq org-agenda-skip-deadline-if-done t)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-agenda-files (list "~/Dropbox/Org/agenda.org"
                         "~/Dropbox/Org/someday.org"
                         "~/Dropbox/Org/birthday.org"
                         "~/Dropbox/Org/books.org"
                         ))

;; warning with appt and notify-send
(setq
  appt-message-warning-time 15 ;; warn 15 min in advance
  appt-display-mode-line t     ;; show in the modeline
  appt-display-format 'window) ;; use our func
(appt-activate 1)              ;; active appt (appointment notification)
(display-time)                 ;; time display is required for this...

;; update appt each time agenda opened

(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)

;; our little façade-function for djcb-popup
(defun appt-display (min-to-app new-time msg)
  (notify-send (format "Appointment in %s minute(s)" min-to-app)
    msg
    "/usr/share/icons/gnome/32x32/status/appointment-soon.png"
    "/usr/share/sounds/ubuntu/stereo/phone-incoming-call.ogg"))

(setq appt-disp-window-function (function appt-display))

;; Nom français des jours et mois affichés dans le calendrier
;; (cf. M-x calendar)
;; (setq european-calendar-style t)
;; (setq calendar-week-start-day 1)
;; (defvar calendar-day-name-array
;;   ["dimanche" "lundi" "mardi" "mercredi" "jeudi" "vendredi" "samedi"])
;; (defvar calendar-day-abbrev-array
;;   ["dim" "lun" "mar" "mer" "jeu" "ven" "sam"])
;; (defvar calendar-month-name-array
;;   ["janvier" "février" "mars" "avril" "mai" "juin"
;;     "juillet" "août" "septembre" "octobre" "novembre" "décembre"])
;; (defvar calendar-month-abbrev-array
;;   ["jan" "fév" "mar" "avr" "mai" "jun"
;;     "jul" "aoû" "sep" "oct" "nov" "déc"])

;; (add-hook 'after-init-hook 'org-agenda-list)


;; (defun toggle-fullscreen ()
;;   (interactive)
;;   (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
;;     '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
;;   (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
;;     '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
;;   )
;; (add-hook 'after-init-hook 'toggle-fullscreen)

(add-to-list 'load-path "~/.emacs.d/yasnippet-0.6.1c")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/yasnippet-0.6.1c/snippets")
(yas/global-mode 1)

;; se rappelle ou je suis dans un fichier
(setq save-place-file "~/.emacs.d/saveplace") ;; keep my ~/ clean
(setq-default save-place t) ;; activate it for all buffers
(require 'saveplace) ;; get the package

;; retourne au dernier endroit changé dans le buffer
(require 'goto-last-change)

;; Indentation du buffer
(defun indent-buffer ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))


;; Numérotation des lignes dans la marge
(require 'linum)
(global-linum-mode 1)
(setq linum-format "%5d")

;; Correction orthographique
(setq ispell-dictionary "fr")
(load-file "~/.emacs.d/flyspell-1.7n.el")

(defun switch-dictionary ()
  "Switch between en and fr dictionaries."
  (interactive)
  (ispell-change-dictionary
    (if (string=
	  (or ispell-local-dictionary ispell-dictionary)
	  "fr")
      "en" "fr"))
  (when flyspell-mode
    (flyspell-delete-all-overlays)
    (flyspell-buffer)))


(defun patch (func pattern patch)
  (fset func (repl (symbol-function func) pattern patch)))

(defun repl (exp pattern patch)
  (cond
    ((null exp) nil)
    ((listp exp)
      (let ((expr (repl (car exp) pattern patch)))
        (if (equal expr pattern)
          `(,@patch ,@(repl (cdr exp) pattern patch))
          (cons expr (repl (cdr exp) pattern patch)))))
    (t exp)))

;; (patch 'LaTeX-label
;;   '(completing-read
;;      (TeX-argument-prompt t nil "What label")
;;      (LaTeX-label-list) nil nil prefix)
;;   '((completing-read
;;       (TeX-argument-prompt t nil "What labelz")
;;       (LaTeX-label-list) nil nil nil nil
;;       (concat prefix (reftex-string-to-label title)))))

;; (eval-after-load 'latex '(patch 'LaTeX-common-initialization
;;                         (quote '("eqnarray" LaTeX-env-label))
;;                         (quote ('("equation" LaTeX-env-label) '("equation*" LaTeX-env-label)))))

;;; Auctex
(add-to-list 'load-path "~/.emacs.d/auctex-11.86")
(load "auctex.el" nil t t)
(setq auto-mode-alist (cons '("\\.tex$" . LaTeX-mode) auto-mode-alist))
(setq TeX-PDF-mode t)
(setq TeX-source-specials-view-emacsclient-flags "-c -no-wait +%%l %%f")
(setq LaTeX-command "latex --shell-escape")

;; indentation correcte des items
(setq LaTeX-item-indent 0)

(add-hook 'LaTeX-mode-hook
  '(lambda ()
     (setq LaTeX-verbatim-environments-local '("tikz"))
     (turn-on-reftex)
     (flyspell-mode)
     (add-to-list 'TeX-output-view-style '("^pdf$" "." "evince %o %(outpage)"))))

(defun latex-accent ()
  (interactive)
  (let ((n 0))
    (save-excursion
      (mapc
        (lambda (e)
          (goto-char (point-min))
          (while (search-forward (car e) nil t)
            (replace-match (cdr e) nil t)
            (setq n (+ 1 n))))
        '(("\\'{e}" . "é")
           ("\\'e" . "é")
           ("\\`{e}" . "è")
           ("\\`e" . "è")
           ("\\`{a}" . "à")
           ("\\`a" . "à")
           ("\\`{u}" . "ù")
           ("\\`u" . "ù")
           ("\\^{e}" . "ê")
           ("\\^e" . "ê")
           ("\\^{o}" . "ô")
           ("\\^o" . "ô")
           ("\\^{u}" . "û")
           ("\\^u" . "û")
           ("\\\"{i}. " "ï")
           ("\\\"i. " "ï")
           ("\\c{c}" . "ç")
           ("\\^{i}" . "î")
           ("\\^i" . "î"))))
    (message (format "Replaced %d occurences" n))))


;; bookmarks
(setq
  bookmark-default-file "~/.emacs.d/bookmarks" ;; keep my ~/ clean
  bookmark-save-flag 1)                        ;; autosave each change

;; TODO faire survivre le processus à la fermeture de emacs
(defun gnome-open (filename)
  (let ((process-connection-type nil))
    (start-process "" nil "/usr/bin/gnome-open" filename)))

(defadvice find-file (around find-or-launch-file)
  "Gnome opens file that emacs can't."
  (if (string-match "\\.pdf$" (ad-get-arg 0))
    (gnome-open (ad-get-arg 0))
    ad-do-it))

(ad-activate 'find-file)

;; pour naviguer facilement entre les buffers avec C-x b
;; affiche la liste des buffers et l'autocomplétion fait le reste
;; BUG ido-execute command marche pas quand c'est la première chose qu'on fait en entrant dans emacs, si on ouvre un fichier avant alors ça marche
;; Use C-f during file selection to switch to regular find-file
;;(require 'ido)

;; (ido-mode t)
;; (setq ido-enable-flex-matching t)
;; (setq ido-execute-command-cache nil)
;; (setq ido-save-directory-list-file "~/.emacs.d/cache/.ido.last")

;; ;; complétion à la ido avec M-x
;; (defun ido-execute-command ()
;;   (interactive)
;;   (call-interactively
;;     (intern
;;       (ido-completing-read
;;         "M-x "
;;         (progn
;;           (unless ido-execute-command-cache
;;             (mapatoms (lambda (s)
;;                         (when (commandp s)
;;                           (setq ido-execute-command-cache
;;                             (cons (format "%S" s) ido-execute-command-cache))))))
;;           ido-execute-command-cache)))))

;; (global-set-key "\M-x" 'ido-execute-command)

;; Autoriser la transparence
(set-frame-parameter (selected-frame) 'alpha '(100 100))
(add-to-list 'default-frame-alist '(alpha 100 100))
(eval-when-compile (require 'cl))
(defun toggle-transparency ()
  (interactive)
  (if (/=
        (cadr (find 'alpha (frame-parameters nil) :key #'car))
        100)
    (set-frame-parameter nil 'alpha '(100 100))
    (set-frame-parameter nil 'alpha '(60 60))))
(global-set-key (kbd "C-c t") 'toggle-transparency)

;; Ouverture des fichiers récents (menu en plus dans la barre de menu)
(require 'recentf)
(recentf-mode 1)
(setq recentf-arrange-by-rules-min-items 0
  recentf-arrange-by-rule-others nil
  recentf-arrange-rules
  '
  (
    ("Elisp files (%d)" ".\\.el\\(.gz\\)?$" "^\\.?emacs-")
    ("Ruby files (%d)" ".\\.rb$")
    ("Scilab files (%d)" ".\\.\\(sci\\|sce\\)$")
    ("Java files (%d)" ".\\.java$")
    ("C/C++ files (%d)" ".\\.c\\(pp\\)?$")
    ("PHP files (%d)" ".\\.\\(php\\|php3\\)$")
    ("Configuration files (%d)" "rc$\\|/\\.")
    ("Ada files (%d)" ".\\.ad[sb]$")
    ("TeX/LaTeX files (%d)" ".\\.\\(tex\\|bib\\|sty\\)$")
    ("Scripts (%d)" ".\\.\\(sh\\|pl\\)$")
    ("Documentation (%d)" "/doc/")
    ("Po files (%d)" "\\.po\\'\\|\\.po\\.")
    ("Lisp files (%d)" ".\\.lisp$")
    )
  recentf-max-saved-items 50
  recentf-max-menu-items 30
  recentf-menu-path nil
  recentf-exclude '(".emacs-customize$"
                     ".newsrc"
                     ".etags$"
                     ".emacs.bmk$"
                     ".bbdb$"
                     ".log$"
                     "^/tmp/")
  recentf-menu-filter 'recentf-arrange-by-rule
  recentf-menu-title "Recentf"
  )

;; (defun recentf-interactive-complete ()
;;   "find a file in the recently open file using ido for completion"
;;   (interactive)
;;   (let* ((all-files recentf-list)
;;           (file-assoc-list (mapcar (lambda (x) (cons (file-name-nondirectory x) x)) all-files))
;;           (filename-list (remove-duplicates (mapcar 'car file-assoc-list) :test 'string=))
;;           (ido-make-buffer-list-hook
;;             (lambda ()
;;               (setq ido-temp-list filename-list)))
;;           (filename (ido-read-buffer "Find Recent File: "))
;;           (result-list (delq nil (mapcar (lambda (x) (if (string= (car x) filename) (cdr x))) file-assoc-list)))
;;           (result-length (length result-list)))
;;     (find-file
;;       (cond
;;         ((= result-length 0) filename)
;;         ((= result-length 1) (car result-list))
;;         ( t
;;           (let ( (ido-make-buffer-list-hook
;;                    (lambda ()
;;                      (setq ido-temp-list result-list))))
;;             (ido-read-buffer (format "%d matches:" result-length))))
;;         ))))


;; ;; pouvoir ouvrir la liste des fichiers récents au clavier
;; ;;(global-set-key "\C-x\C-r" 'recentf-open-files-complete)
;; (global-set-key "\C-x\C-r" 'recentf-interactive-complete)


;; Prise en charge de la molette de la souris
;; Utilisée seule, la rotation de la molette provoque un défilement de
;; 5 lignes par cran. Combinée à la touche Shift, le défilement est
;; réduit à une ligne. Combinée à la touche Control, le défilement
;; s'effectue page (1 hauteur de fenêtre) par page.
(require 'mwheel)
(mouse-wheel-mode 1)


;; Bind last command to F12
(global-set-key [(f12)] 'repeat-complex-command)

;; pouvoir utiliser la complétion sous emacs en ignorant la
;; casse ça évite de passer à côté d'une alternative parce qu'on ne se
;; souvenait pas qu'il y avait un caractère en majuscules...
(setq completion-ignore-case t)

;; filenames too, to browse with dired for example...
(setq read-file-name-completion-ignore-case t)

;;; dired, dired-x and co
(add-hook 'dired-load-hook
  (function (lambda ()
              (load "dired-x")
              ;; Set global variables here.  For example:
              ;; (setq dired-guess-shell-gnutar "gtar")
              )))
(add-hook 'dired-mode-hook
  (function (lambda ()
              ;; Set buffer-local variables here.  For example:
              ;; (dired-omit-mode 1)
              )))

;; (require 'dired+)

(defvar dired-sort-map (make-sparse-keymap))
(define-key dired-mode-map "s" dired-sort-map)

(mapc
  (lambda (elt)
    (define-key dired-sort-map (car elt)
      `(lambda ()
	(interactive)
	(dired-sort-other
	  (concat dired-listing-switches
	    (unless (string-match "-r" dired-actual-switches)
	      " -r") ,(cadr elt))))))
  '(("n" "")
     ("x" " -X")
     ("s" " -S")
     ("t" " -t")
     ("d" " --group-directories-first")))

;;; Autoinsert mode
;; l'auto-insert permet d'insérer selon l'extension d'un
;; fichier un contenu de fichier statique
(require 'autoinsert)
(auto-insert-mode t) ; Adds hook to find-files-hook
(setq auto-insert-directory (expand-file-name "~/.emacs.d/autoinsert/"))
(setq auto-insert-query nil)
(setq auto-insert-alist
  '(
     ("\\.rb$" . ["autoinsert.ruby" (lambda () (goto-char (point-max)))])
     ("\\.sh$"   . ["autoinsert.bash" (lambda () (goto-char (point-max)))])
     ("\\.tex$" . ["autoinsert.tex" (lambda () (goto-line 19))])
     ))
(setq auto-insert 'other)


(defun change-to-utf-8 ()
  (interactive)
  (set-buffer-file-coding-system 'utf-8-unix)
  )


(global-set-key [(f2)] 'change-to-utf-8)

(require 'starter-kit-ruby)
;;(require 'ruby-electric)

;;; shell-toggle
(autoload 'shell-toggle "shell-toggle"
  "Toggles between the shell buffer and whatever buffer you are editing."
  t)
(autoload 'shell-toggle-cd "shell-toggle"
  "Pops up a shell-buffer and insert a \"cd <file-dir>\" command." t)
(global-set-key [M-f1] 'shell-toggle)
(global-set-key [C-f1] 'shell-toggle-cd)

;; minibuffer history
;;(savehist-mode t)

;; Always add a final newline
(setq require-final-newline t)
(add-hook 'write-file-functions 'delete-trailing-whitespace)

;; Afficher l'heure dans la barre d'état (format 24 heures)
(display-time)
(setq display-time-24hr-format 1)

;; Selon les règles typographiques françaises, le point final d'une
;; phrase n'est suivi que d'un seul espace (contre deux dans la
;; tradition anglo-saxonne). Il est utile qu'Emacs le sache pour
;; formater correctement les textes.
(setq sentence-end-double-space nil)

;; automatically indent wherever I am
(global-set-key (kbd "RET") 'newline-and-indent)

;; Drive out the mouse when it's too near to the cursor.
(mouse-avoidance-mode 'animate)


(setq x-select-enable-clipboard t        ; copy-paste should work ...
  interprogram-paste-function            ; ...with...
  'x-cut-buffer-or-selection-value)      ; ...other X clients

;; quit, no prompt
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

;; don't warn when killing running processes
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
    kill-buffer-query-functions))

;; Make URLs in comments/strings clickable
(add-hook 'find-file-hooks 'goto-address-prog-mode)

;;; hippie-expand
(global-set-key (kbd "S-SPC") 'hippie-expand)
(global-set-key (kbd "C-S-SPC") '(lambda () (interactive) (hippie-expand -1)))

(setq hippie-expand-try-functions-list
  '(yas/hippie-try-expand
     try-complete-file-name-partially
     try-complete-file-name
     ;;try-expand-list
     ;;try-expand-line
     try-expand-dabbrev-visible
     try-expand-dabbrev
     try-expand-dabbrev-all-buffers
     try-expand-dabbrev-from-kill))

;; Customized Emacs Lisp mode
(require 'eldoc)
(autoload 'turn-on-eldoc-mode "eldoc" nil t)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)

(add-hook 'emacs-lisp-mode-hook
  (lambda()
    (setq mode-name "ELisp")
    (local-set-key (kbd "C-x E")
      '(lambda()(interactive)
         (let ((debug-on-error t))
           (cond
             (mark-active
               (call-interactively 'eval-region)
               (message "Region evaluated!")
               (setq deactivate-mark t))
             (t
               (eval-buffer)
               (message "Buffer evaluated!"))))))
    (linum-mode t)
    (setq lisp-indent-offset 2) ; indent with two spaces, enough for lisp
    ;;(turn-on-auto-fill)
    (require 'folding nil 'noerror)
    (set (make-local-variable 'hippie-expand-try-functions-list)
      '(yas/hippie-try-expand
         try-complete-file-name-partially
         try-complete-file-name
         try-expand-dabbrev-visible
         try-expand-dabbrev
         try-complete-lisp-symbol-partially
         try-complete-lisp-symbol))
    ;;marquer les caractères au delà de 80 caractères
    (font-lock-add-keywords nil
      '(("^[^;\n]\\{80\\}\\(.*\\)$" 1 font-lock-warning-face prepend)))
    (font-lock-add-keywords nil
      '(("\\<\\(FIXME\\|TODO\\|BUG\\)"
          1 font-lock-warning-face prepend)))
    (font-lock-add-keywords nil
      '(("\\<\\(add-hook\\|setq\\)\\>"
          1 font-lock-keyword-face prepend)))))

;; enable narrow-to-region binding
(put 'narrow-to-region 'disabled nil)

;; Delete the selected region when something is typed or with DEL
(delete-selection-mode t)

;; Text selection highlighted by default on Emacs 23
;;(transient-mark-mode t)

;; non interactive function in apropos
;; Make C-h a act as C-u C-h a
(setq apropos-do-all t)

;; Use system trash (for emacs 23)
(setq delete-by-moving-to-trash t)

;; Make scripts executable on save
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; This was installed by package-install.el.
;; This provides support for the package system and
;; interfacing with ELPA, the package archive.
;; Move this code earlier if you want to reference
;; packages in your .emacs.
(when
  (load
    (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))


;; additional menu
(require 'easymenu)
(setq my-encoding-map (make-sparse-keymap "Encoding Menu"))
(easy-menu-define my-encoding-menu my-encoding-map
  "Encoding Menu."
  '("Change File Encoding"
     ["UTF8 - Unix (LF)" (set-buffer-file-coding-system 'utf-8-unix) t]
     ["UTF8 - Mac (CR)" (set-buffer-file-coding-system 'utf-8-mac) t]
     ["UTF8 - Win (CR+LF)" (set-buffer-file-coding-system 'utf-8-dos) t]
     ["--" nil nil]
     ["Shift JIS - Mac (CR)" (set-buffer-file-coding-system 'sjis-mac) t]
     ["Shift JIS - Win (CR+LF)" (set-buffer-file-coding-system 'sjis-dos) t]
     ["--" nil nil]
     ["EUC - Unix (LF)" (set-buffer-file-coding-system 'euc-jp-unix) t]
     ["JIS - Unix (LF)" (set-buffer-file-coding-system 'junet-unix) t]
     ))
(define-key-after menu-bar-file-menu [my-file-separator]
  '("--" . nil) 'kill-buffer)
(define-key-after menu-bar-file-menu [my-encoding-menu]
  (cons "File Encoding" my-encoding-menu) 'my-file-separator)

;; TESTING

(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
    ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
    (t (self-insert-command (or arg 1)))))
(global-set-key "%" 'match-paren)

;; but give the emacs window a still good shape !
;; emacs -mm starts maximized
;; (setq initial-frame-alist '((width . 90) (height . 42))) ; .Xdefaults
;; (setq initial-frame-alist
;;   '((left . 1) (top . 1)
;;      (width . 176) (height . 46)))


;; Lorsqu'une ligne est plus large que la fenêtre d'affichage, je veux
;; qu'Emacs me l'affiche sur autant de lignes que nécessaire plutôt que de
;; masquer la partie qui dépasse à droite de l'écran. Pour que ce comportement
;; vaille en toute circonstance, il est nécessaire de fixer deux variables :
;; - truncate-lines : comportement dans un tampon occupant toute la largeur de
;;   la fenêtre
;; - truncate-partial-width-windows : comportement dans un tampon n'occupant
;;   qu'une fraction de la largeur de la fenêtre (par exemple, après un
;;   découpage horizontal C-x 3).
(setq truncate-lines nil)
(setq truncate-partial-width-windows nil)


;; Suppression du formatage sur N colonnes (comme la fonction n'existe pas,
;; l'astuce consiste à définir temporairement une largeur de ligne extrêmement
;; grande et de formater le paragraphe sur cette base).
(defun remove-hard-wrap-paragraph ()
  "Replace newline chars in current paragraph by single spaces."
  (interactive)
  (let ((fill-column 10000000))
    (fill-paragraph nil)))

;; Fonction équivalente à la précédente appliquée à la région sélectionnée et
;; non plus au paragraphe courant.
(defun remove-hard-wrap-region (start end)
  "Replace newline chars in region by single spaces."
  (interactive "r")
  (let ((fill-column 10000000))
    (fill-region start end)))

;; TODO tester si une région existe
;; « M-S-q » (i.e. M-Q) : supprime le formatage du paragraphe courant (la
;; combinaison « M-q », qui existe par défaut, le formatant)
(global-set-key (kbd "M-Q") 'remove-hard-wrap-paragraph)


;; Thu Sep  9 15:33:21 2010
;; FIXME marche qu'avec le curseur sur la parenthèse fermante.
(defadvice show-paren-function (after my-echo-paren-matching-line activate)
  "If a matching paren is off-screen, echo the matching line."
  (when (char-equal (char-syntax (char-before (point))) ?\))
    (let ((matching-text (blink-matching-open)))
      (when matching-text
        (message matching-text)))))

;; IDEA tip of the day http://bitbucket.org/scfrazer/dot_emacs/src/tip/init.el

;; (defun smart-tab ()
;;   "This smart tab is minibuffer compliant: it acts as usual in
;;    the minibuffer. Else, if mark is active, indents region. Else if
;;    point is at the end of a symbol, expands it. Else indents the
;;    current line."
;;   (interactive)
;;   (if (minibufferp)
;;     (unless (minibuffer-complete)
;;       (dabbrev-expand nil))
;;     (if (smart-tab-must-expand 0)
;;       (smart-expand-function)
;;       (smart-indent))))

;; (defun smart-indent ()
;;   "Indents region if mark is active, or current line otherwise."
;;   (interactive)
;;   (if mark-active
;;     (indent-region (region-beginning)
;;       (region-end))
;;     (indent-for-tab-command)))

;; (defun smart-tab-must-expand (&optional prefix)
;;   "If PREFIX is \\[universal-argument], answers no.
;;    Otherwise, analyses point position and answers."
;;   (unless (or (consp prefix)
;;          mark-active)
;;     (looking-at "\\_>")))

(global-set-key [\C-home] 'beginning-of-buffer)
(global-set-key [\C-end] 'end-of-buffer)

;; fuck occur and word isearch
(global-set-key (kbd "M-s") 'backward-kill-word)

;; http://steve.yegge.googlepages.com/my-dot-emacs-file
;; someday might want to rotate windows if more than 2 of them
(defun swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (if (not (= (count-windows) 2))
    (message "You need exactly 2 windows to do this.")
    (let* ((w1 (first (window-list)))
            (w2 (second (window-list)))
            (b1 (window-buffer w1))
            (b2 (window-buffer w2))
            (s1 (window-start w1))
            (s2 (window-start w2)))
      (set-window-buffer w1 b2)
      (set-window-buffer w2 b1)
      (set-window-start w1 s2)
      (set-window-start w2 s1)
      (other-window 1))))

(defun my-reindent-then-newline-and-indent-and-indent-sexp ()
  "Reindent current line, insert newline, then indent the new line.
Move backward out of one level of parentheses.
Indent each line of the list starting just after point."
  (interactive "*")
  (reindent-then-newline-and-indent)
  (save-excursion
    (backward-up-list)
    (indent-sexp)))

;; redo support, yay
;; (require 'redo)
;; (global-set-key (kbd "C-ç") 'redo)


;; (org-babel-do-load-languages
;;   'org-babel-load-languages
;;   '(
;;      (latex . t) ; this is the entry to activate LaTeX
;;      (sh . t)
;;      ))

(setq Info-default-directory-list
  (append
    '("/home/sylvain/dotemacs/dotemacs/.emacs.d/auctex-11.86/doc"
       "/home/sylvain/dotemacs/org-mode/doc")
    Info-default-directory-list))


(defun shutdown-emacs-server () (interactive)
  (when (not (eq window-system 'x))
    (message "Initializing x windows system.")
    (x-initialize-window-system)
    (when (not x-display-name) (setq x-display-name (getenv "DISPLAY")))
    (select-frame (make-frame-on-display x-display-name '((window-system . x))))
    )
  (let ((last-nonmenu-event nil)(window-system
                                  "x"))(save-buffers-kill-emacs)))

;; (setq eval-expression-print-length 100)
;; (setq eval-expression-print-level 10)

;; This is sweet!  right-click, get a list of functions in the source
;; file you are editing
;; (http://emacs.wordpress.com/2007/01/24/imenu-with-a-workaround/#comment-51)
(global-set-key [mouse-3] `imenu)

;; marche pas avec magit par exemple...
(defun smart-tab ()
  "This smart tab is minibuffer compliant: it acts as usual in
    the minibuffer. Else, if mark is active, indents region. Else if
    point is at the end of a symbol, expands it. Else indents the
    current line."
  (interactive)
  ;; change this to (if (minibufferp) when I can finally ditch the
  ;; old version of emacs at work.
  (if (string-match "Minibuf" (buffer-name))
    (unless (minibuffer-complete)
      (dabbrev-expand nil))
    (if mark-active
      (indent-region (region-beginning)
        (region-end))
      ;; add an underscore after the '\\' when I can finally ditch
      ;; the old version of emacs at work.
      (if (looking-at "\\>")
        (dabbrev-expand nil)
        (indent-for-tab-command)))))

;; (global-set-key [tab] 'smart-tab)

;; writing hippie-expand flyspell
;; (defun try-expand-all-abbrevs (old)
;;   "Try to expand word before point according to all abbrev tables.
;; The argument OLD has to be nil the first call of this function, and t
;; for subsequent calls (for further possible expansions of the same
;; string).  It returns t if a new expansion is found, nil otherwise."
;;   (if (not old)
;;     (progn
;;       (he-init-string (he-dabbrev-beg) (point))
;;       (setq he-expand-list
;; 	(and (not (equal he-search-string ""))
;; 	  (mapcar (function (lambda (sym)
;; 			      (if (and (boundp sym) (vectorp (eval sym)))
;; 				(abbrev-expansion (downcase he-search-string)
;; 				  (eval sym)))))
;; 	    (append '(local-abbrev-table
;; 		       global-abbrev-table)
;; 	      abbrev-table-name-list))))))
;;   (while (and he-expand-list
;; 	   (or (not (car he-expand-list))
;; 	     (he-string-member (car he-expand-list) he-tried-table t)))
;;     (setq he-expand-list (cdr he-expand-list)))
;;   (if (null he-expand-list)
;;     (progn
;;       (if old (he-reset-string))
;;       ())
;;     (progn
;;       (he-substitute-string (car he-expand-list) t)
;;       (setq he-expand-list (cdr he-expand-list))
;;       t)))


;; (setq word "fenetre")
;; (process-send-string ispell-process "%\n") ;put in verbose mode
;; (process-send-string ispell-process (concat "^" word "\n"))
;; ;; wait until ispell has processed word
;; (while (progn
;; 	 (accept-process-output ispell-process)
;; 	 (not (string= "" (car ispell-filter)))))
;; (setq ispell-filter (cdr ispell-filter))
;; (if (consp ispell-filter)
;;   (setq poss (ispell-parse-output (car ispell-filter))))

;; poss
;; (cond
;;   ((or (eq poss t) (stringp poss))
;;     ;; don't correct word
;;     t)
;;   ((null poss)
;;     ;; ispell error
;;     (error "Ispell: error in Ispell process"))
