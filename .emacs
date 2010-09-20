(add-to-list 'load-path (expand-file-name "~/.emacs.d"))
(add-to-list 'load-path (expand-file-name "~/dotemacs/org-mode/lisp"))
(add-to-list 'load-path (expand-file-name "~/dotemacs/anything-config"))
(add-to-list 'load-path (expand-file-name "~/dotemacs/anything-config/extensions"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/auto-install"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/apel-10.7"))
;; (load "elscreen" "ElScreen" t)

(require 'auto-install)

(require 'instant)

(require 'notify-send)

;; abandonne le minibuffer quand on le quitte
(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

;; ouvre un buffer en sudo via tramp
(defun sudo-edit (&optional arg)
  (interactive "p")
  (let ((tramp-prefix "/sudo:root@localhost:"))
    (if (and
          (= arg 1)
          (buffer-file-name)
          (not (string-match "^/sudo:root@localhost:" buffer-file-name)))
      (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                   (ido-read-file-name "File: "))))))

(defvar trans-term-p t "Check if fullscreen is on or off")

(defun non-trans-term ()
  (interactive)
  (set-frame-parameter nil 'alpha '(100 100))
  (jump-to-register 'r)
  (tool-bar-mode 1)
  (menu-bar-mode 1)
  )

(defun trans-term ()
  (interactive)
  (frame-configuration-to-register 'r)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (delete-other-windows)
  (set-frame-size (selected-frame) 70 40)
  (set-frame-parameter nil 'alpha '(50 50))
  )

(defun toggle-trans-term ()
  (interactive)
  (setq trans-term-p (not trans-term-p))
  (if trans-term-p
    (non-trans-term)
    (trans-term)))

(global-set-key (kbd "C-x C-p") 'find-file-at-point)

(defun find-temp-file (extension)
  (interactive "sExtension: ")
  (if (> (length extension) 0) (setq extension (concat "." extension)))
  (find-file (concat (make-temp-file "foo") extension)))

(global-set-key (kbd "C-x C-t") 'find-temp-file)

(global-set-key (kbd "<C-kp-6>") 'enlarge-window-horizontally)
(global-set-key (kbd "<C-kp-4>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-kp-2>") 'enlarge-window)
(global-set-key (kbd "<C-kp-8>") 'shrink-window)

(require 'linkd)


;; anything
(require 'xml)
(require 'anything-startup)
(setq anything-command-map-prefix-key "C-x C-a")

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
  (start-process-shell-command "updatedb process" nil
    "updatedb -l 0 -U /media/THISKEY/ -o $HOME/.locate.db"))

(defun anything-c-locate-thiskey-init ()
  "Initialize async locate process for `anything-c-source-locate'."
  (start-process-shell-command "locate-thiskey-process" nil
    (format (concat "locate -e -d " (expand-file-name "~/.locate.db") " -i -r \"%s\"")
      anything-pattern)))

(defvar anything-c-source-locate-thiskey
  '((name . "Locate in THISKEY")
     (candidates . anything-c-locate-thiskey-init)
     (type . file)
     (requires-pattern . 3)
     (delayed))
  "Find files matching the current input pattern with locate.")


;; magit
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


;; sélection avec les flèches dans buffer-list
;;(global-set-key (kbd "C-x C-b") 'electric-buffer-list)

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
      ("Org" ;; all org-related buffers
        (mode . org-mode))
      ("TeX/LaTeX"
        (or
          (mode . latex-mode)
          (name . "^\\.tex$")))
      ("Mail"
        (or ;; mail-related buffers
          (mode . message-mode)
          (mode . mail-mode)
          ;; etc.;; all your mail related modes
          ))
      ("Dired"
                                        ; dired related buffers
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
          ;; etc
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
  (if (member major-mode '(ruby-mode emacs-lisp-mode scheme-mode lisp-mode
                            c-mode c++-mode objc-mode
                            latex-mode plain-tex-mode
                            python-mode))
    (indent-region (region-beginning) (region-end) nil)))

(defadvice yank-pop (after indent-region activate)
  (if (member major-mode '(ruby-mode emacs-lisp-mode scheme-mode lisp-mode
                            c-mode c++-mode objc-mode
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
                                    (line-beginning-position 2))))))

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
           (line-beginning-position 2))))))


;;; la commande kill supprime automatiquement les espaces d'indentations si besoin
(defadvice kill-line (before check-position activate)
  (if (member major-mode '(emacs-lisp-mode scheme-mode lisp-mode
                            c-mode c++-mode objc-mode
                            latex-mode plain-tex-mode
                            ruby-mode python-mode))
    (if (and (eolp) (not (bolp)))
      (progn (forward-char 1)
        (just-one-space 0)
        (backward-char 1)))))

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
(set (make-local-variable 'comment-auto-fill-only-comments) t)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; efface tous les espaces et sauts de ligne avec un seul backspace
(setq backward-delete-char-untabify-method (quote all))

;;move between windows with meta-arrows
(windmove-default-keybindings 'meta)

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
  backup-by-copying t     ;; and copy them here
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
(global-set-key (kbd "s-s o") ;; .emacs
  (lambda () (interactive)
    (find-file-existing "/media/THISKEY/Documents/Org/TODO.org")))
(global-set-key (kbd "C-x à") 'delete-other-windows)
(global-set-key (kbd "C-x C-à") 'delete-other-windows)

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

;; don't let Customize mess with my .emacs
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;; color theme
(add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0")
(require 'color-theme)
(color-theme-initialize)
(color-theme-subtle-hacker)

;; org-mode
(require 'org-install)

(setq org-todo-keywords
  '("TODO" "|" "CANCELLED" "DONE"))

(setq org-capture-templates
  '(("t" "Todo" entry
      (file+headline "/media/THISKEY/Documents/Org/someday.org" "Tâches")
      "* TODO %?\n  OPENED: %U")
     ("p" "Programming" entry
       (file+headline "/media/THISKEY/Documents/Org/someday.org" "Programming")
       "* TODO %?\n  OPENED: %U")
     ("e" "Event" entry
       (file+headline "/media/THISKEY/Documents/Org/agenda.org" \,)
       "* EVENT %?")
     ("qu" "Quote" entry
       (file+headline "/media/THISKEY/Documents/Org/quotes.org" "")
       "* %^{description}%?\n  OPENED: %U")
     ("a" "Anniv" entry
       (file+headline "/media/THISKEY/Documents/Org/birthday.org" "")
       "* %^{Birthday}t Anniversaire de %^{prompt}!\n")
     ("b" "Livres empruntés")
     ("bu" "Bibliothèque Universitaire" entry
       (file+headline "/media/THISKEY/Documents/Org/books.org"
  "Empruntés")
       "* %?\n  BORROWED: %u\n  À RENDRE: %(add-days 15)")
     ("bl" "Bibliothèque du labo" entry
       (file+headline "/media/THISKEY/Documents/Org/books.org"
	 "Empruntés")
       "* %?\n  BORROWED: %u\n  À RENDRE: %(add-days 365)")))

(defun add-days (days)
  (format-time-string
    (car org-time-stamp-formats)
    (time-add (current-time) (seconds-to-time (* 24 3600 days)))))

(define-key global-map "\C-cc" 'org-capture)

;; Ne mettre qu'une seule étoile devant les titres
;; FIXME marche pas quand on change le color-theme
;; (setq org-hide-leading-stars t)

;; logging
(setq org-log-done 'time)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-agenda-files (list "/media/THISKEY/Documents/Org/agenda.org"
                         "/media/THISKEY/Documents/Org/someday.org"
                         "/media/THISKEY/Documents/Org/birthday.org"
			 "/media/THISKEY/Documents/Org/books.org"
                         ))

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
(setq ispell-dictionary "francais")
(load-file "~/.emacs.d/flyspell-1.7n.el")

;; Cedet
;;(load-file "~/.emacs.d/cedet-1.0pre6/common/cedet.el")
;;(global-ede-mode 1)                      ; Enable the Project management system
;;(semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion
;;(global-srecode-minor-mode 1)            ; Enable template insertion menu

;; ECB
;;(add-to-list 'load-path "~/.emacs.d/ecb-2.40")
;;(require 'ecb)

;; Scilab
;;(add-to-list 'load-path "~/.emacs.d/scilabelisp")
;;(load "scilab-startup")
;;(setq auto-mode-alist (cons '("\\(\\.sci$\\|\\.sce$\\)" . scilab-mode) auto-mode-alist))
;;(add-hook 'scilab-mode-hook '(lambda () (setq fill-column 90)))

;; Auctex
(add-to-list 'load-path "~/.emacs.d/auctex-11.85")
(load "auctex.el" nil t t)
(setq auto-mode-alist (cons '("\\.tex$" . LaTeX-mode) auto-mode-alist))
(setq TeX-PDF-mode t)
(setq TeX-source-specials-view-emacsclient-flags "-c -no-wait +%%l %%f")
(setq LaTeX-command "latex --shell-escape")

;; indentation correcte des items
(setq LaTeX-item-indent 0)

(add-hook 'LaTeX-mode-hook
  '(lambda ()
     (turn-on-reftex)
     (flyspell-mode)
     (add-to-list 'TeX-output-view-style '("^pdf$" "." "evince %o %(outpage)"))))

(defun latex-accent ()
  (interactive)
  (save-excursion
    (let
      ((assoc
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
            ("\\^i" . "î")))
        (n 0))
      (progn
        (mapc
          (lambda (e)
            (while
              (search-forward (car e) nil t)
              (progn
                (replace-match (cdr e) nil t)
                (setq n (+ 1 n)))))
          assoc)
        (message (format "Replaced %d occurences" n))))))


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

;; dired customizations
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
(define-key dired-sort-map "s" (lambda () "sort by Size" (interactive) (dired-sort-other (concat dired-listing-switches "S"))))
(define-key dired-sort-map "x" (lambda () "sort by eXtension" (interactive) (dired-sort-other (concat dired-listing-switches "X"))))
(define-key dired-sort-map "t" (lambda () "sort by Time" (interactive) (dired-sort-other (concat dired-listing-switches "t"))))
(define-key dired-sort-map "n" (lambda () "sort by Name" (interactive) (dired-sort-other dired-listing-switches)))
(define-key dired-sort-map "d" (lambda () "sort by name grouping Dirs" (interactive) (dired-sort-other (concat " --group-directories-first " dired-listing-switches))))

;; Désactivation des boites de dialogue
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(tool-bar-mode -1)

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

;;shell-toggle
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

;; Selon les règles typographiques françaises, le point final d'une phrase
;; n'est suivi que d'un seul espace (contre deux dans la tradition
;; anglo-saxonne). Il est utile qu'Emacs le sache pour formater correctement
;; les textes.
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

;; Make URLs in comments/strings clickable
(add-hook 'find-file-hooks 'goto-address-prog-mode)

;; hippie-expand
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

;; M-RET to keep writing comments
(global-set-key (kbd "M-RET") comment-line-break-function)

(add-hook 'emacs-lisp-mode-hook
  (lambda()
    (setq mode-name "ELisp")
    (local-set-key (kbd "C-x E")
      '(lambda()(interactive)
         (let ((debug-on-error t))
           (cond
             (mark-active
               (call-interactively 'eval-region)
               (message "Region evaluated!"))
             (t
               (eval-buffer)
               (message "Buffer evaluated!"))))))
    (linum-mode t)
    (setq lisp-indent-offset 2) ; indent with two spaces, enough for lisp
    (turn-on-auto-fill)
    (require 'folding nil 'noerror)
    (set (make-local-variable 'hippie-expand-try-functions-list)
      '(yas/hippie-try-expand
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
;; (setq initial-frame-alist '((width . 90) (height . 42))) ; .Xdefaults
(setq initial-frame-alist
  `((left . 0) (top . 51)
     (width . 176) (height . 46)))


;; Nom français des jours et mois affichés dans le calendrier
;; (cf. M-x calendar)
(setq european-calendar-style t)
(setq calendar-week-start-day 1)
(defvar calendar-day-name-array
  ["dimanche" "lundi" "mardi" "mercredi" "jeudi" "vendredi" "samedi"])
(defvar calendar-day-abbrev-array
  ["dim" "lun" "mar" "mer" "jeu" "ven" "sam"])
(defvar calendar-month-name-array
  ["janvier" "février" "mars" "avril" "mai" "juin"
    "juillet" "août" "septembre" "octobre" "novembre" "décembre"])
(defvar calendar-month-abbrev-array
  ["jan" "fév" "mar" "avr" "mai" "jun"
    "jul" "aoû" "sep" "oct" "nov" "déc"])


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
    (fill-paragraph nil)
    )
  )

;; Fonction équivalente à la précédente appliquée à la région sélectionnée et
;; non plus au paragraphe courant.
(defun remove-hard-wrap-region (start end)
  "Replace newline chars in region by single spaces."
  (interactive "r")
  (let ((fill-column 10000000))
    (fill-region start end)
    )
  )

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

;; isearch-mode-map http://bitbucket.org/birkenfeld/dotemacs/src/tip/init.el

;; Put the cursor in an intelligent place when searching
(add-hook 'isearch-mode-end-hook 'custom-goto-match-beginning)
(defun custom-goto-match-beginning ()
  "Use with isearch hook to end search at first char of match"
  (when (and
          isearch-forward
          (not mark-active)
          (not isearch-mode-end-hook-quit)) (goto-char isearch-other-end)))

(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
               (regexp-quote isearch-string))))))

;; function at point!!
(global-set-key  [f1] 'find-function-at-point)


;; always revert buffers if their files change on disk to reflect new changes
;; (global-auto-revert-mode 1)

;; shortcut for reverting a buffer
(global-set-key (kbd "C-x C-r") 'revert-buffer)


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

;; TODO add custom hippie hook depending on the major mode
(add-hook 'python-mode-hook
  (lambda ()
    (set (make-local-variable 'hippie-expand-try-functions-list)
      '(yas/hippie-try-expand
         py-complete
         try-expand-dabbrev-visible
         try-expand-dabbrev))))

;; fuck occur and word isearch
(global-set-key (kbd "M-s") 'backward-kill-word)

;;; http://steve.yegge.googlepages.com/my-dot-emacs-file
;; someday might want to rotate windows if more than 2 of them
(defun swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (cond ((not (= (count-windows) 2))
          (message "You need exactly 2 windows to do this."))
    (t
      (let* ((w1 (first (window-list)))
              (w2 (second (window-list)))
              (b1 (window-buffer w1))
              (b2 (window-buffer w2))
              (s1 (window-start w1))
              (s2 (window-start w2)))
        (set-window-buffer w1 b2)
        (set-window-buffer w2 b1)
        (set-window-start w1 s2)
        (set-window-start w2 s1)))))

;; TODO is it better than the one i have?
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME." (interactive "sNew name: ")
  (let ((name (buffer-name))
         (filename (buffer-file-name)))
    (if (not filename)
      (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
        (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))


(defun move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR." (interactive "DNew directory: ")
  (let* ((name (buffer-name))
          (filename (buffer-file-name))
          (dir
            (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
          (newname (concat dir "/" name)))

    (if (not filename)
      (message "Buffer '%s' is not visiting a file!" name)
      (progn (copy-file filename newname 1
               (delete-file filename)
               (set-visited-file-name newname)
               (set-buffer-modified-p nil)
               t)))))

  (defun my-find-thing-at-point ()
    "Find variable, function or file at point."
    (interactive)
    (cond ((not (eq (variable-at-point) 0))
	    (call-interactively 'describe-variable))
      ((function-called-at-point)
	(call-interactively 'describe-function))
      (t (find-file-at-point))))



(defun my-reindent-then-newline-and-indent-and-indent-sexp ()
  "Reindent current line, insert newline, then indent the new line.
Move backward out of one level of parentheses.
Indent each line of the list starting just after point."
  (interactive "*")
  (reindent-then-newline-and-indent)
  (save-excursion
    (backward-up-list)
    (indent-sexp)))

;; quick bind to f1 to try out
(defmacro bind-to-f1 (&rest prog)
  `(global-set-key [f1]
     (lambda ()
       (interactive)
       ,@prog)))

(bind-to-f1 (my-reindent-then-newline-and-indent-and-indent-sexp))

(defun my-beginning-of-line ()
  (interactive)
  (message "mlkdsf")
  (let ((old-point (point)))
    (beginning-of-line-text)
    (and (= (point) old-point) (beginning-of-line))))

(defun my-beginning-of-line ()
  (interactive)
  (if (bolp)
    (beginning-of-line-text)
    (beginning-of-line)))

(setq desktop-buffers-not-to-save
  (concat "\\(" "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^/tmp"
    "\\)$"))

(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)

;; redo support, yay
;; (require 'redo)
;; (global-set-key (kbd "C-ç") 'redo)


(org-babel-do-load-languages
  'org-babel-load-languages
  '(
     (latex . t) ; this is the entry to activate LaTeX
     (sh . t)
     ))

(setq Info-default-directory-list
  (append
    '("/home/sylvain/dotemacs/dotemacs/.emacs.d/auctex-11.85/doc"
       "/home/sylvain/dotemacs/org-mode/doc")
    Info-default-directory-list))

(defun my-change-dictionary ()
  "Change the dictionary."
  (interactive)
  (let ((dict (or ispell-local-dictionary ispell-dictionary)))
    (setq dict (if (string= dict "fr_FR") "en_US" "fr_FR"))
    (message "Switched to %S" dict)
    (sit-for 0.4)
    (ispell-change-dictionary dict)
    (when flyspell-mode
      (flyspell-delete-all-overlays)
      (flyspell-buffer))))

