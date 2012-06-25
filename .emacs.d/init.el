;; Set path to .emacs.d
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path dotfiles-dir)

;; Set path to dependencies
(setq site-lisp-dir "~/Dropbox/emacs/site-lisp/")
(add-to-list 'load-path site-lisp-dir)

;; add all directories in site-lisp
(dolist (path (directory-files site-lisp-dir t "[^\\.]\\|\\(\\.\\{3,\\}\\)"))
  (if (file-directory-p path)
      (add-to-list 'load-path path)))

(add-to-list 'load-path (concat dotfiles-dir "vendor"))
(dolist (path (directory-files
               (concat dotfiles-dir "vendor") t "[^\\.]\\|\\(\\.\\{3,\\}\\)"))
  (if (file-directory-p path)
      (add-to-list 'load-path path)))

(defun load-file-to-list (file)
  "Return a list of FORM found in file `file'."
  (if (and (file-exists-p file)
           (file-readable-p file))
      (with-temp-buffer
        (insert-file-contents file)
        (let ((marker (copy-marker 0))
              form-list form)
          (while (ignore-errors (setq form (read marker)))
            (setq form-list (cons form form-list)))
          (reverse form-list)))))

(require 'init-fill)
(require 'init-dired)
(require 'init-isearch)
(require 'init-boss-key)
(require 'init-erc)
(require 'init-magit)
(require 'init-find-file)
(require 'init-ispell)
(require 'init-latex)
(require 'init-desktop)
(require 'init-midnight)
(require 'init-helm)
(require 'init-matlab)
(require 'init-yasnippet)
(require 'init-org)
(require 'init-elisp)
(require 'init-auctex)
(require 'init-ibuffer)
(require 'init-bindings)

(load-library "paren")
(show-paren-mode 1)

(require 'epa)
(epa-file-enable)

(require 'expand-region)
(global-set-key (kbd "C-à") 'er/expand-region)
(global-set-key (kbd "C-M-à") 'er/contract-region)

;; Numérotation des lignes dans la marge
(require 'linum)
(global-linum-mode 1)
(setq linum-format "%5d")

(autoload 'yaml-mode "yaml-mode")
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))

;;; cmake-mode
(require 'cmake-mode)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                ("\\.cmake\\'" . cmake-mode))
              auto-mode-alist))

;; markdown
(autoload 'markdown-mode "markdown-mode")
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))

;;; ruby
(autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process" t)
(autoload 'inf-ruby-setup-keybindings "inf-ruby" "" t)
(eval-after-load 'ruby-mode
  '(progn
     (add-hook 'ruby-mode-hook 'inf-ruby-setup-keybindings)
     (add-hook 'ruby-mode-hook
               (lambda ()
                 (define-key ruby-mode-map (kbd "RET")
                   'reindent-then-newline-and-indent)))))

;; tidy.el
(autoload 'tidy-buffer "tidy" "Run Tidy HTML parser on current buffer" t)
(autoload 'tidy-parse-config-file "tidy" "Parse the `tidy-config-file'" t)
(autoload 'tidy-save-settings "tidy" "Save settings to `tidy-config-file'" t)
(autoload 'tidy-build-menu  "tidy" "Install an options menu for HTML Tidy." t)

(require 'mwheel)
(mouse-wheel-mode 1)

;; pas de file<2> quand 2 buffers ont le même nom
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets
      uniquify-after-kill-buffer-p t)

(require 'saveplace)
(setq save-place-file "~/.emacs.d/.saveplace")
(setq-default save-place t)

;; retourne au dernier endroit changé dans le buffer
(require 'goto-last-change)
(global-set-key (kbd "C-x C-_") 'goto-last-change)

;; Ouverture des fichiers récents (menu en plus dans la barre de menu)
(require 'recentf)
(recentf-mode 1)
(setq recentf-arrange-by-rules-min-items 0
      recentf-arrange-by-rule-others nil
      recentf-arrange-rules
      '(("Elisp files (%d)" ".\\.el\\(.gz\\)?$" "^\\.?emacs-")
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
        ("Lisp files (%d)" ".\\.lisp$"))
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
      recentf-menu-title "Recentf")


;; winner-mode
(winner-mode 1)
(setq winner-boring-buffers
      '("*Completions*"
        "*anything for files*"
        "*anything find-file*"
        "*anything complete*"
        "*Ibuffer*"
        "*Calendar*"
        "*anything*"))

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

;; paste in term
(require 'term)
(define-key term-raw-map (kbd "C-y") 'term-paste)

;; adding packages source
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t))

;; notify events
(when (>= emacs-major-version 24)
  (require 'notifications))

(require 'ffap)
(add-to-list 'ffap-alist '(latex-mode . ffap-bib-latex-mode))

;; find pdf at a ref which has the same name in `pdfs-directory'
(defvar pdfs-directory nil
  "Directory to look for pdf files.")

(put 'pdfs-directory 'safe-local-variable 'string-or-null-p)

(defun ffap-bib-latex-mode (name)
  (and pdfs-directory (concat pdfs-directory name ".pdf")))

(defun my-find-thing-at-point ()
  "Find variable, function or file at point."
  (interactive)
  (cond ((not (eq (variable-at-point) 0))
         (call-interactively 'describe-variable))
        ((function-called-at-point)
         (call-interactively 'describe-function))
        (t (if (ffap-file-at-point)         ;trick to ffap when point is at the end of a link
               (find-file-at-point)
             (save-excursion
               (backward-char 2)
               (find-file-at-point))))))

(global-set-key (kbd "C-x C-p") 'my-find-thing-at-point)

;; loading zenburn theme
(load-theme 'zenburn t)

;; BUG: require is cyan. Loading zenburn-theme.el fixes this
(load "zenburn-theme")


(require 'vc-git-check-status)
(require 'org-context)

;; load file settings if any
(ignore-errors (load-file "~/Dropbox/emacs/org-context-settings.el"))

(require 'vc-git-commit-all)

(require 'init-autoinsert)

(require 'org-bib-workflow)

(unless (server-running-p)
  (server-start))

(add-to-list 'default-frame-alist
             '(font . "-unknown-Inconsolata-bold-normal-normal-*-*-*-*-*-m-0-iso10646-1"))

;; Highlight current line
(global-hl-line-mode 1)

;; Désactivation des boites de dialogue
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(tool-bar-mode -1)
(setq inhibit-startup-screen t)

;; unified diff format and no whitespace when using `diff'
(setq diff-switches "-u -w")

;; no limit on how many lines to keep in *Messages* buffer
(setq message-log-max t)

;; no fringe on the right
(set-fringe-mode '(8 . 0))

;; no fringe in minibuffer
(set-window-fringes (minibuffer-window) 0 0)

;; abandonne le minibuffer quand on le quitte
(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

(defun update-locate-database ()
  "Update locate databases"
  (interactive)
  (and (file-exists-p "/media/THISKEY")
       (set-process-sentinel
        (start-process-shell-command
         "updatedb process" nil
         (mapconcat
          'identity
          '("updatedb -l 0"
            "-U /media/THISKEY/"
            "--add-prunepaths \"/media/THISKEY/.Trash-1000 /media/THISKEY/.Trash-1001\""
            "-o $HOME/.locate.db")
          " "))
        (lambda (process event)
          (minibuffer-message (if (string= "finished\n" event)
                                  "Locate database updated!"
                                "Updating locate database failed!"))))))

;; update locate database when idle during 20 sec
(run-with-idle-timer 20 nil 'update-locate-database)

(require 'dbus)

(defun THISKEY-dbus-signal-handler (service id args)
  "Resurrect THISKEY opened buffers when it is plugged"
  (when (string= "THISKEY" (cadr args))
    (let ((desktop-load-locked-desktop t))
      (save-window-excursion
        (desktop-read)))
    (run-with-idle-timer 20 nil 'update-locate-database)
    (run-with-idle-timer 20 nil
                         (lambda ()
                           (run-hooks 'midnight-hook)))
    (minibuffer-message "Mounting THISKEY, desktop-read")))

(when (fboundp 'dbus-register-signal)
  (dbus-register-signal
   :session
   "org.gtk.Private.GduVolumeMonitor"
   "/org/gtk/Private/RemoteVolumeMonitor"
   "org.gtk.Private.RemoteVolumeMonitor"
   "MountAdded"
   'THISKEY-dbus-signal-handler))

(defun emacs-process-p (pid)
  "If pid is the process ID of an emacs process, return t, else nil.
Also returns nil if pid is nil."
  (when pid
    (let* ((cmdline-file (concat "/proc/" (int-to-string pid) "/cmdline")))
      (when (file-exists-p cmdline-file)
        (with-temp-buffer
          (insert-file-contents-literally cmdline-file)
          (goto-char (point-min))
          (and (search-forward "emacs" nil t) t))))))

(defadvice desktop-owner (after pry-from-cold-dead-hands activate)
  "Don't allow dead emacsen to own the desktop file."
  (when (not (emacs-process-p ad-return-value))
    (setq ad-return-value nil)))


;;; Prevent Extraneous Tabs
(setq-default indent-tabs-mode nil)

;; auto byte-compile init.el
(defun byte-compile-init-file ()
  "Recompile init.el file if newer than its corresponding .elc
  file. If some errors are found, ask for confirmation. This
  function is designed to be placed in
  `kill-emacs-query-functions' hook."
  (or (not (file-newer-than-file-p
            "~/.emacs.d/init.el"
            "~/.emacs.d/init.elc"))
      (byte-compile-file "~/.emacs.d/init.el")
      (yes-or-no-p "Your init file contains errors; Exit anyway?")))

(add-hook 'kill-emacs-query-functions 'byte-compile-init-file)

;; auto-save with non-visiting buffer is too rigid
(defun save-scratch-buffer ()
  "Create a backup of scratch buffer"
  (and (get-buffer "*scratch*")
       (with-current-buffer "*scratch*"
         (and (buffer-modified-p)
              (write-file
               (concat (file-name-as-directory
                        (assoc-default "*scratch*" backup-directory-alist 'string-match))
                       "scratch-buffer-backup.el"))))))

(add-hook 'kill-emacs-hook 'save-scratch-buffer)

;; suit les liens vers les systèmes de contrôle de versions
(setq vc-follow-symlinks t)

(defalias 'yes-or-no-p 'y-or-n-p)

;; Laisser le curseur en place lors d'un défilement par pages. Par
;; défaut, Emacs place le curseur en début ou fin d'écran selon le
;; sens du défilement.
(setq scroll-preserve-screen-position t)

;; sélection avec SHIFT
;;(custom-set-variables '(pc-selection-mode t nil (pc-select)))

(defun rename-current-file-or-buffer ()
  (interactive)
  (if (not (buffer-file-name))
      (call-interactively 'rename-buffer)
    (let ((file (buffer-file-name)))
      (save-buffer)
      (with-temp-buffer
        (set-buffer (dired-noselect file))
        (dired-do-rename)
        (kill-buffer nil))))
  nil)


;; history navigation
(eval-after-load "comint"
  '(progn
     (define-key comint-mode-map [(control ?p)] 'comint-previous-input)
     (define-key comint-mode-map [(control ?n)] 'comint-next-input)))


;; indenter automatiquement le code collé :
(mapc
 (lambda (func)
   (eval `(defadvice ,func (after indent-region activate)
            (if (memq major-mode '(ruby-mode emacs-lisp-mode scheme-mode
                                             lisp-interaction-mode sh-mode
                                             lisp-mode c-mode c++-mode objc-mode
                                             latex-mode plain-tex-mode
                                             python-mode matlab-mode))
                (indent-region (region-beginning) (region-end) nil)))))
 '(yank yank-pop))

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
  (setq last-buffer-we-cut-from (or buffer-file-name (buffer-name))))


;; la commande kill supprime automatiquement les espaces
;; d'indentations si besoin
(defadvice kill-line (before check-position activate)
  (if (member major-mode '(emacs-lisp-mode
                           lisp-interaction-mode
                           scheme-mode lisp-mode
                           c-mode c++-mode objc-mode
                           latex-mode plain-tex-mode
                           ruby-mode python-mode))
      (if (and (eolp) (not (bolp)))
          (progn (forward-char 1)
                 (just-one-space 0)
                 (backward-char 1)))))

;; copy when in read only buffer
(setq kill-read-only-ok t)

(defun my-beginning-of-line ()
  (interactive)
  (if (bolp)
      (beginning-of-line-text)
    (beginning-of-line)))

(global-set-key (kbd "C-a") 'my-beginning-of-line)

(defun transpose-buffers (arg)
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

;; quick bind to f1 to try out
(defmacro bind-to-f1 (&rest prog)
  `(global-set-key [f1]
                   (lambda ()
                     (interactive)
                     ,@prog)))


;; echo keystrokes quickly
(setq echo-keystrokes 0.1)

;; efface tous les espaces et sauts de ligne avec un seul backspace
(setq backward-delete-char-untabify-method (quote all))



;; custom frame title
(modify-frame-parameters nil '((name . nil)))
(setq frame-title-format
      '(:eval
        (concat "Emacs: "
                (or
                 buffer-file-name
                 (and (eq major-mode 'dired-mode)
                      (expand-file-name
                       (if (listp dired-directory)
                           (car dired-directory)
                         dired-directory)))
                 (buffer-name)))))

;; Non au défilement qui accélère
(setq mouse-wheel-progressive-speed nil)

;;; scratch

;; put something different in the scratch buffer
(setq initial-scratch-message
      ";; scratch buffer created -- happy hacking\n\n")

(defun insert-in-scratch-buffer (string)
  (with-current-buffer (get-buffer-create "*scratch*")
    (goto-char (point-max))
    (and (not (bolp)) (insert "\n"))
    (insert
     (with-temp-buffer
       (insert (mapconcat
                'identity
                (split-string string "\n+")
                " "))
       (let ((fill-colunm 70)
             (fill-prefix ";; "))
         (goto-char (point-min))
         (insert ";; ")
         (fill-region (point-min) (point-max)))
       (buffer-string))
     "\n\n")
    (set-buffer-modified-p nil)))

(and (executable-find "ruby")
     (set-process-filter
      (start-process-shell-command
       "msg in scratch buffer"
       nil
       "ruby ~/Dropbox/scripts/SCMB.rb")
      (lambda (process string)
        (insert-in-scratch-buffer string))))

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

;; don't let Customize mess with my .emacs
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

;; Indentation du buffer
(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (untabify-buffer)
  (indent-buffer)
  (delete-trailing-whitespace))

(defun delete-trailing-whitespace-safe ()
  "Delete trailing whitespace if file is not version controlled
or version controlled but untracked."
  (and
   (and (buffer-file-name)
        (or
         (not (vc-backend (buffer-file-name)))
         (eq (vc-backend (buffer-file-name)) 'none))
        (delete-trailing-whitespace))
   nil))

;; if indent-tabs-mode is off, untabify before saving
(defun untabify-safe ()
  "Untabify buffer if it is not VC or untracked in VC and if
`indent-tabs-mode' is nil."
  (and
   (and (buffer-file-name)
        (or
         (not (vc-backend (buffer-file-name)))
         (eq (vc-backend (buffer-file-name)) 'none))
        (not indent-tabs-mode)
        (untabify (point-min) (point-max)))
   nil))

(defun cleanup-buffer-safe ()
  (interactive)
  (untabify-safe)
  (delete-trailing-whitespace-safe))

(add-hook 'before-save-hook 'cleanup-buffer-safe)

(defun patch (func pattern patch)
  "Patch a function definition by replacing `pattern' by `patch'."
  (and (byte-code-function-p (symbol-function func))
       (error "Symbol `%s' is bound to a byte compiled function." func))
  (fset func (repl (symbol-function func) pattern patch)))

(defun repl (exp pattern patch)
  (cond
   ((null exp) nil)
   ((listp exp)
    (let ((expr (repl (car exp) pattern patch)))
      (cons
       (if (equal expr pattern) patch expr)
       (repl (cdr exp) pattern patch))))
   (t exp)))

;; bookmarks
(setq
 bookmark-default-file "~/.emacs.d/bookmarks" ;; keep my ~/ clean
 bookmark-save-flag 1)                        ;; autosave each change

;; Autoriser la transparence
(set-frame-parameter (selected-frame) 'alpha '(100 100))
(add-to-list 'default-frame-alist '(alpha 100 100))

(defun toggle-transparency ()
  (interactive)
  (if (/= (cadr (assoc 'alpha (frame-parameters nil))) 100)
      (set-frame-parameter nil 'alpha '(100 100))
    (set-frame-parameter nil 'alpha '(60 60))))


;; Bind last command to F12
(global-set-key [(f12)] 'repeat-complex-command)

;; pouvoir utiliser la complétion sous emacs en ignorant la
;; casse ça évite de passer à côté d'une alternative parce qu'on ne se
;; souvenait pas qu'il y avait un caractère en majuscules...
(setq completion-ignore-case t)

;; filenames too, to browse with dired for example...
(setq read-file-name-completion-ignore-case t)

;;; Autoinsert mode
;; l'auto-insert permet d'insérer selon l'extension d'un
;; fichier un contenu de fichier statique

(defun change-to-utf-8 ()
  (interactive)
  (set-buffer-file-coding-system 'utf-8-unix))


(global-set-key [(f2)] 'change-to-utf-8)

;; minibuffer history
;;(savehist-mode t)

;; Always add a final newline
(setq require-final-newline t)

;; Afficher l'heure dans la barre d'état (format 24 heures)
(display-time)
(setq display-time-24hr-format 1)

;; Selon les règles typographiques françaises, le point final d'une
;; phrase n'est suivi que d'un seul espace (contre deux dans la
;; tradition anglo-saxonne). Il est utile qu'Emacs le sache pour
;; formater correctement les textes.
(setq sentence-end-double-space nil)

;; Drive out the mouse when it's too near to the cursor.
(mouse-avoidance-mode 'animate)


(setq x-select-enable-clipboard t        ; copy-paste should work ...
      interprogram-paste-function            ; ...with...
      'x-cut-buffer-or-selection-value)      ; ...other X clients

;; don't warn when quitting emacs with running processes
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

;; don't warn when killing running processes
(delq 'process-kill-buffer-query-function
      kill-buffer-query-functions)

;; Make URLs in comments/strings clickable
(add-hook 'find-file-hooks 'goto-address-prog-mode)

;;; hippie-expand
(global-set-key (kbd "S-SPC") 'hippie-expand)
(global-set-key (kbd "C-S-SPC") (lambda () (interactive) (hippie-expand -1)))

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

;; M-g g but for characters
(defun interactive-goto-char (point)
  (interactive "nGoto char: ")
  (goto-char point))

(global-set-key (kbd "M-g c") 'interactive-goto-char)

;; Make scripts executable on save
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; open bash-fc-* files from fc command or C-x C-e in terminal in sh-mode
(add-to-list 'auto-mode-alist '("bash-fc-[0-9]+\\'" . sh-mode))

;; replace-string and replace-regexp need a key binding
(global-set-key (kbd "C-c s") 'replace-string)
(global-set-key (kbd "C-c r") 'replace-regexp)

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

(setq Info-default-directory-list
      (append
       (list (expand-file-name "~/dotemacs/dotemacs/.emacs.d/auctex-11.86/doc")
             (expand-file-name "~/dotemacs/dotemacs/.emacs.d/vendor/org-mode/doc"))
       Info-default-directory-list))


(defun shutdown-emacs-server ()
  (interactive)
  (when (not (eq window-system 'x))
    (message "Initializing x windows system.")
    (x-initialize-window-system)
    (unless x-display-name (setq x-display-name (getenv "DISPLAY")))
    (select-frame (make-frame-on-display x-display-name '((window-system . x)))))
  (let ((last-nonmenu-event nil)
        (window-system "x"))
    (save-buffers-kill-emacs)))

;; This is sweet!  right-click, get a list of functions in the source
;; file you are editing
;; (http://emacs.wordpress.com/2007/01/24/imenu-with-a-workaround/#comment-51)
(global-set-key [mouse-3] `imenu)


(defun intelligent-close ()
  "quit a frame the same way no matter what kind of frame you are on.

This method, when bound to C-x C-c, allows you to close an emacs frame the
same way, whether it's the sole window you have open, or whether it's
a \"child\" frame of a \"parent\" frame.  If you're like me, and use emacs in
a windowing environment, you probably have lots of frames open at any given
time.  Well, it's a pain to remember to do Ctrl-x 5 0 to dispose of a child
frame, and to remember to do C-x C-x to close the main frame (and if you're
not careful, doing so will take all the child frames away with it).  This
is my solution to that: an intelligent close-frame operation that works in
all cases (even in an emacs -nw session).

Stolen from http://www.dotemacs.de/dotfiles/BenjaminRutt.emacs.html."
  (interactive)
  (if (eq (car (visible-frame-list)) (selected-frame))
      ;;for parent/master frame...
      (if (> (length (visible-frame-list)) 1)
          ;;close a parent with children present
          (delete-frame (selected-frame))
        ;;close a parent with no children present
        (save-buffers-kill-emacs))
    ;;close a child frame
    (delete-frame (selected-frame))))

(global-set-key "\C-x\C-c" 'intelligent-close) ;forward reference

(defun to-scr (srt)
  "print in scratch buffer"
  (with-current-buffer "*scratch*"
    (save-excursion
      (goto-char (point-max))
      (or (bolp) (insert "\n"))
      (insert
       (if (stringp srt)
           srt
         (prin1-to-string srt))))))

(defmacro to-scr2 (srt)
  "print in scratch buffer"
  `(with-current-buffer "*scratch*"
     (save-excursion
       (goto-char (point-max))
       (or (bolp) (insert "\n"))
       (insert
        (format "%s: %s" (symbol-name ',srt) ,srt)))))


(global-set-key (kbd "C-j")
                (lambda ()
                  (interactive)
                  (kill-line 0)))

(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
