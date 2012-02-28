(add-to-list 'load-path (expand-file-name "~/.emacs.d"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/org-mode/lisp"))

(add-to-list 'default-frame-alist
             '(font . "-unknown-Inconsolata-bold-normal-normal-*-*-*-*-*-m-0-iso10646-1"))


;; Font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; Désactivation des boites de dialogue
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(tool-bar-mode -1)

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

;; ouvre un buffer en sudo via tramp
(defun th-find-file-sudo (file)
  "Opens FILE with root privileges."
  (interactive "F")
  (set-buffer
   (find-file
    (concat "/sudo::"
            (expand-file-name file)))))

(defadvice find-file (around th-find-file activate)
  "Open FILENAME using tramp's sudo method if it's read-only."
  (if (and (not (file-writable-p (ad-get-arg 0)))
           (not (file-remote-p (ad-get-arg 0)))
           (y-or-n-p (concat "File "
                             (ad-get-arg 0)
                             " is read-only.  Open it as root? ")))
      (th-find-file-sudo (ad-get-arg 0))
    ad-do-it))


(defun find-temp-file (extension)
  "quick find file in /tmp"
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

;;; el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(require 'el-get)

;;; anything
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/anything-config"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/anything-config/extensions"))
(require 'anything-config)
(require 'anything-match-plugin)
(require 'anything-complete)
(anything-read-string-mode 1)

(setq anything-command-map-prefix-key "C-x C-a")

(setq anything-su-or-sudo "sudo")

(setq anything-c-locate-command "locate -e -b -i -r \"%s\"")

(defun anything-for-files-prefered-list ()
      `(anything-c-source-ffap-line
        anything-c-source-ffap-guesser
        anything-c-source-buffers-list
        anything-c-source-recentf
        anything-c-source-bookmarks
        anything-c-source-file-cache
        anything-c-source-files-in-current-dir+
        ,@(if (file-exists-p "/media/THISKEY") '(anything-c-source-locate-thiskey))
        anything-c-source-locate))

(defun anything-for-files ()
  "Preconfigured `anything' for opening files.
ffap -> recentf -> buffer -> bookmark -> file-cache -> files-in-current-dir -> locate."
  (interactive)
  (anything-other-buffer (anything-for-files-prefered-list) "*anything for files*"))

(defun update-locate-database ()
  "Update locate databases"
  (interactive)
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
     (message (if (string= "finished\n" event)
                  "Locate database updated!"
                "Updating locate database failed!")))))

;; update locate database when idle during 10 sec
(run-with-idle-timer 10 nil 'update-locate-database)

(ignore-errors

(require 'dbus)

(defun THISKEY-dbus-signal-handler (service id args)
  "Resurrect THISKEY opened buffers when it is plugged"
  (when (string= "THISKEY" (cadr args))
    (let ((desktop-load-locked-desktop t))
      (save-window-excursion
        (desktop-read)))
    (run-with-idle-timer 10 nil 'update-locate-database)
    (run-with-idle-timer 10 nil
                         (lambda ()
                           (run-hooks 'midnight-hook)))
    (message "Mounting THISKEY, desktop-read")))

  (when (fboundp 'dbus-register-signal)
    (dbus-register-signal
     :session
     "org.gtk.Private.GduVolumeMonitor"
     "/org/gtk/Private/RemoteVolumeMonitor"
     "org.gtk.Private.RemoteVolumeMonitor"
     "MountAdded"
     'THISKEY-dbus-signal-handler)))

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

(defun anything-c-locate-thiskey-init ()
  "Initialize async locate process for `anything-c-source-locate'."
  (start-process-shell-command
   "locate-thiskey-process" nil
   (format (concat "locate -e -b -d " (expand-file-name "~/.locate.db") " -i -r \"%s\"")
           anything-pattern)))

(defvar anything-c-source-locate-thiskey
  '((name . "Locate in THISKEY")
    (candidates . anything-c-locate-thiskey-init)
    (type . file)
    (requires-pattern . 3)
    (delayed))
  "Find files matching the current input pattern with locate.")

;; boss key!

(defvar boss-window-configuration nil
  "Window configuration to switch to when the boss comes!")

(defvar my-window-configuration nil
  "Window configuration to switch back to!")

(defvar boss-mode nil)

(defun boss-save nil
  "Define current window configuration as boss's one"
  (interactive)
  (setq boss-window-configuration (current-window-configuration))
  (message "Boss window configuration set to current!"))

(defun boss-toggle nil
  "Toggle boss window configuration. Switch to *scratch* buffer
if `boss-window-configuration' is nil."
  (interactive)
  (if boss-mode
      (set-window-configuration my-window-configuration)
    (setq my-window-configuration (current-window-configuration))
    (if boss-window-configuration
        (set-window-configuration boss-window-configuration)
      (delete-other-windows)
      (switch-to-buffer "*scratch*")))
  (setq boss-mode (null boss-mode)))

(global-set-key (kbd "²") 'boss-toggle)
(global-set-key (kbd "C-²") 'boss-save)


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

;;; Prevent Extraneous Tabs
(setq-default indent-tabs-mode nil)

;; M-RET to keep writing comments, clashes with auctex mode
;; (global-set-key (kbd "M-RET") comment-line-break-function)

;; (defadvice indent-for-tab-command (around tab-completion activate)
;;   (cond
;;     ((minibufferp)
;;       (minibuffer-complete))
;;     ((looking-at "\\>")
;;       (hippie-expand nil))
;;     (t ad-do-it)))

;;; magit
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/magit"))
(require 'magit)
(global-set-key "\C-ci" 'magit-status)

;; (eval-after-load 'magit
;;   '(progn
;;      (set-face-foreground 'magit-diff-add "green3")
;;      (set-face-foreground 'magit-diff-del "red3")
;;      (when (and (not window-system) (not (daemonp)))
;;        (set-face-background 'magit-item-highlight "white"))))

(defun mapconcatend (func list separator last-separator)
  "Like mapconcat but the last separator can be specified. Useful
when building sentence like blah, blih, bloh and bluh."
  (cond
   ((null list) "")
   ((cdr (cdr list))
    (concat (funcall func (car list)) separator
            (mapconcatend func (cdr list) separator last-separator)))
   ((cdr list) (concat
                (funcall func (car list))
                last-separator
                (funcall func (cadr list))))
   (t (funcall func (car list)))))

(defvar magit-check-sections
  '((".*" (unstaged . "unstaged changes")
     (unpushed . "unpushed commits"))))

;; warn when untracked files, unpushed commits or changes
(defun magit-check-unfinished ()
  (autoload 'remove-if-not "cl-seq")
  (let ((bl (buffer-list)))
    (while (and bl
                (or (not (string-match "^\\*magit: \\(.*\\)\\*$" (buffer-name (car bl))))
                    (let* ((git-repo-name (match-string 1 (buffer-name (car bl))))
                           (item-list (assoc-default git-repo-name
                                                     magit-check-sections
                                                     'string-match))
                           (unfinished
                            (with-current-buffer (car bl)
                              (remove-if-not
                               (lambda (section)
                                 (let ((sec (magit-find-section (list (car section)) magit-top-section)))
                                   (and sec (> (length (magit-section-children sec)) 0))))
                               item-list))))
                      (or (not unfinished)
                          (yes-or-no-p
                           (format "You have %s in %s; Exit anyway?"
                                   (mapconcatend 'cdr unfinished ", " " and ")
                                   git-repo-name))))))
      (setq bl (cdr bl)))
    (null bl)))

(add-to-list 'kill-emacs-query-functions 'magit-check-unfinished)


;; auto-save with non-visiting buffer is too rigid
(defun save-scratch-buffer ()
  "Create a backup of scratch buffer"
  (and (get-buffer "*scratch*")
       (with-current-buffer "*scratch*"
         (and (buffer-modified-p)
              (write-file (let ((alist backup-directory-alist)
                                elt backup-directory)
                            (while alist
                              (setq elt (pop alist))
                              (if (string-match (car elt) "*scratch*")
                                  (setq backup-directory (cdr elt)
                                        alist nil)))
                            (concat (file-name-as-directory
                                     backup-directory) "scratch-buffer-backup.el")))))))

(add-hook 'kill-emacs-hook 'save-scratch-buffer)

(defun backup-scratch-buffer ()
  (with-current-buffer "*scratch*"
    (let ((alist backup-directory-alist)
          elt backup-directory)
      (while alist
        (setq elt (pop alist))
        (if (string-match (car elt) "*scratch*")
            (setq backup-directory (cdr elt)
                  alist nil)))
      (setq buffer-file-name
            (concat (file-name-as-directory backup-directory)
                    "*scratch*.el"))
      (backup-buffer))
    (setq buffer-file-name nil)))



;; suit les liens vers les systèmes de contrôle de versions
(setq vc-follow-symlinks t)

;; notify events
(ignore-errors (require 'notifications nil t))

;;; erc
;; check channels
(erc-track-mode t)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477"))
(setq erc-autojoin-channels-alist
      '(("freenode.net" "#emacs" "#ruby-lang" "#ruby.fr" "#ruby"
         "#git-fr" "#emacsfr" "#linux-fr" "#debianfr" "#org-mode-fr")))

(erc-match-mode 1)
(setq erc-keywords '("magit" "koans" "rubywarrior" " org" "?"))

(defun my-notify-erc (match-type nickuserhost message)
  "Notify when a message is received."
  (notifications-notify
   :title (format "%s in %s"
                  ;; Username of sender
                  (car (split-string nickuserhost "!"))
                  ;; Channel
                  (or (erc-default-target) "#unknown"))
   :body (cond
          ((eq match-type 'current-nick)
           (if (string-match "^[Tt]hisirs" message)
               "is talking to you!"
             "is talking about you!"))
          ((and (eq match-type 'keywords)
                (string-match "?" message))
           (and (string-match "?$" message)
                (concat "is asking a question!\n" message)))
          (t
           (replace-regexp-in-string "[\t\n ]+" " " message)))
   :icon "emacs-snapshot"
   :timeout -1))

(add-hook 'erc-text-matched-hook 'my-notify-erc)

(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))

(setq erc-join-buffer 'bury)

(defun erc-start-or-switch ()
  "Connect to ERC, or switch to last active buffer"
  (interactive)
  (if (get-buffer "irc.freenode.net:6667") ;; ERC already active?
      (erc-track-switch-buffer 1) ;; yes: switch to last active
    (when (y-or-n-p "Start ERC? ") ;; no: maybe start ERC
      (erc :server "irc.freenode.net" :port 6667 :nick "thisirs"))))


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
      (save-buffer)
      (with-temp-buffer
        (set-buffer (dired-noselect file))
        (dired-do-rename)
        (kill-buffer nil))))
  nil)


;;; matlab mode
;; (autoload 'matlab-mode "matlab" "Enter MATLAB mode." t)
;; (setq auto-mode-alist (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))
;; (autoload 'matlab-shell "matlab" "Interactive MATLAB mode." t)
(add-to-list 'load-path "~/.emacs.d/vendor/matlab-emacs")
(require 'matlab-load)
(setq matlab-shell-command-switches '("-nodesktop" "-nosplash"))

(add-hook 'matlab-mode-hook
          (lambda ()
            (setq fill-column 76)                       ; where auto-fill should wrap
            (turn-on-auto-fill)
            (setq-default matlab-functions-have-end t)
            (setq-default matlab-indent-function-body t)
            (setq matlab-change-current-directory t)
            (setq-default matlab-indent-function t)))

;; history navigation
(eval-after-load "comint"
  '(progn
     (define-key comint-mode-map [(control ?p)] 'comint-previous-input)
     (define-key comint-mode-map [(control ?n)] 'comint-next-input)))


;;; IBuffer
(require 'ibuffer)

(defadvice ibuffer-diff-with-file (around ibuffer-diff-two-buffers activate)
  (require 'diff)
  (let ((marked-bufs (ibuffer-get-marked-buffers)))
    (if (eq (length marked-bufs) 2)
      (diff (car marked-bufs) (cadr marked-bufs))
      ad-do-it)))

;; don't show empty groups
(setq ibuffer-show-empty-filter-groups nil)

(defun ibuffer-next-buffer-aux (list)
  (if (null list)
      (error "No buffers!"))
  (let ((current-buffer (ibuffer-current-buffer t))
        (list0 list)
        (list1 list)
        (ibuffer-buffer-list
         (mapcar #'car
                 (ibuffer-current-state-list))))
    (while (not (equal current-buffer (car list0)))
      (setq list0 (cdr list0)))
    (setq list0 (cdr list0))
    (while (and list0 (not (memql (car list0) ibuffer-buffer-list)))
      (setq list0 (cdr list0)))
    (if list0 (setq next-buffer (car list0))
      (while (not (memql (car list1) ibuffer-buffer-list))
        (setq list1 (cdr list1)))
      (setq next-buffer (car list1)))
    (ibuffer-jump-to-buffer (buffer-name next-buffer))))

(defun ibuffer-next-buffer ()
  "Jump to next buffer in IBuffer according to `(buffer-list)'"
  (interactive)
  (ibuffer-next-buffer-aux (buffer-list)))

(defun ibuffer-previous-buffer ()
  "Jump to previous buffer in IBuffer according to `(buffer-list)'"
  (interactive)
  (ibuffer-next-buffer-aux
   (reverse (buffer-list))))

(define-key ibuffer-mode-map (kbd "C-b") 'ibuffer-next-buffer)
(define-key ibuffer-mode-map (kbd "C-f") 'ibuffer-previous-buffer)

(defun ibuffer-next-saved-filter-groups-aux (list)
  (if (null ibuffer-saved-filter-groups)
      (error "No saved filters"))
  (let ((next-filter-group) (list0 list))
    (while (and (null next-filter-group) list0)
      (if (equal ibuffer-filter-groups (cdr (car list0)))
          (setq next-filter-group (car (car list0))))
      (setq list0 (cdr list0)))
    (setq list0 (or list0 list))
    (setq ibuffer-filter-groups (cdr (car list0)))
    (message "Switched to \"%s\" filter group!" (car (car list0))))
  (setq ibuffer-hidden-filter-groups nil)
  (ibuffer-update nil t))

(defun ibuffer-next-saved-filter-groups ()
  (interactive)
  (ibuffer-next-saved-filter-groups-aux ibuffer-saved-filter-groups))

(defun ibuffer-previous-saved-filter-groups ()
  (interactive)
  (ibuffer-next-saved-filter-groups-aux
   (reverse ibuffer-saved-filter-groups)))

(define-key ibuffer-mode-map (kbd "C-M-n") 'ibuffer-next-saved-filter-groups)
(define-key ibuffer-mode-map (kbd "C-M-p") 'ibuffer-previous-saved-filter-groups)

(define-key ibuffer-mode-map (kbd "C-g") 'ibuffer-quit)

(defadvice ibuffer (around ibuffer-point-to-most-recent activate)
  "Open ibuffer with cursour pointed to second most recent buffer
name"
  (let ((recent-buffer-name (buffer-name)))
    ad-do-it
    (ibuffer-jump-to-buffer recent-buffer-name)
    (ibuffer-next-buffer)))

(defun find-projects (dir)
  "Return a list of all directories containing a not hidden git repo"
  (let ((list
         (and (file-directory-p (concat dir "/.git"))
              (not (file-exists-p (concat dir "/.hidden")))
              (cons dir nil))))
    (apply 'append list
           (mapcar
            (lambda (path)
              (if (file-directory-p path)
                  (find-projects path)))
            ;; avoiding . and ..
            (directory-files dir t "[^\\.]\\|\\(\\.\\{3,\\}\\)")))))


(defun make-ibuffer-projects-list (prefix &rest dir-list)
  "Return a list whose elements are of the form ((`prefixdir' (filename . `directory')"
  (mapcar
   (lambda (dir)
     (list (concat prefix (file-name-nondirectory dir))
           `(filename . ,dir)))
   (apply 'append
          (mapcar
           (lambda (dir)
             (and (file-directory-p dir)
                  (nreverse (find-projects dir))))
           dir-list))))

(setq ibuffer-saved-filter-groups
      `(("default"
         ,@(make-ibuffer-projects-list "Project: "
                                       (concat (getenv "HOME") "/dotemacs"))
         ,@(make-ibuffer-projects-list "Project on THISKEY: "
                                       "/media/THISKEY/programming")
         ("Thèse"
          (or
           (filename . "/media/THISKEY/Documents/These")))
         ("Current paper"
          (or
           (filename . "/media/THISKEY/Documents/These/ARTICLE_REVUE")))
         ("TP IMAGE 2011/2012"
          (or
           (filename . "/media/THISKEY/enseignements/2011-2012/")))
         ("Org"
          (mode . org-mode))
         ("TeX/LaTeX"
          (or
           (mode . latex-mode)
           (name . "\\.bib$")
           (name . "\\.tex$")))
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
           (mode . c++-mode)
           (mode . perl-mode)
           (mode . python-mode)
           (mode . emacs-lisp-mode)
           (mode . ruby-mode)
           (mode . sh-mode)
           (mode . matlab-mode)
           (name . "^\\*scratch\\*$")
           (name . "^\\*Messages\\*$")
           ))
         ("ERC" (mode . erc-mode))
         ("crap" (name . "^\\*.*\\*$")))
        ("Elisp-mode"
         (".el.gz elisp files"
          (name . "\\.el\\.gz$"))
         ("Elisp files"
          (or
           (mode . emacs-lisp-mode)
           (name . "^\\*scratch\\*$")
           (name . "^\\*Messages\\*$"))))
        ("Ruby-mode"
         ("Ruby"
          (or
           (mode . inf-ruby-mode)
           (mode . ruby-mode))))
        ("Matlab-mode"
         ("Matlab"
          (or
           (filename . "\\.m$")
           (name . "^\\*MATLAB\\*$"))))))

;; Use human readable Size column instead of original one
(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (cond
   ((> (buffer-size) (* 1024 1024))
    (format "%7.1fM" (/ (buffer-size) (* 1024 1024.0))))
   ((> (buffer-size) 1024)
    (format "%7.1fk" (/ (buffer-size) 1024.0)))
   (t (format "%8d" (buffer-size)))))

(setq ibuffer-formats
      '((mark modified read-only " "
              (name 18 18 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " " filename-and-process)
        (mark " "
              (name 16 -1)
              " " filename)))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

(global-set-key (kbd "C-x C-b") 'ibuffer)

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

;; find pdf at a ref which has the same name in `pdfs-directory'
(add-to-list 'ffap-alist '(latex-mode . ffap-bib-latex-mode))

(defun ffap-bib-latex-mode (name)
  (and (boundp 'pdfs-directory)
       (stringp pdfs-directory)
       (concat pdfs-directory name ".pdf")))

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

(define-key isearch-mode-map (kbd "C-h") 'isearch-mode-help)

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

;; ouverture rapide avec la touche windows
(global-set-key (kbd "s-s s") ;; scratch
                (lambda () (interactive) (switch-to-buffer "*scratch*")))
(global-set-key (kbd "s-s e") ;; .emacs
                (lambda () (interactive) (find-file (file-truename "~/.emacs.d/init.el"))))
(global-set-key (kbd "s-s m") ;; messages
                (lambda () (interactive) (switch-to-buffer "*Messages*")))

(global-set-key (kbd "C-x à") 'delete-other-windows)
(global-set-key (kbd "C-x C-à") 'delete-other-windows)
(global-set-key (kbd "C-,") 'other-window)

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

;;; desktop-mode
;; save a list of open files in ~/.emacs.desktop

;; Save frame from patch
(if (boundp 'desktop-save-frames)
    (setq desktop-save-frames t))

(setq desktop-load-locked-desktop t)

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

(setq desktop-files-not-to-save
      "\\(^/[^/:]*:\\|(ftp)$\\)\\|\\(^/tmp/\\)\\|\\(.gpg$\\)")
(setq desktop-buffers-not-to-save
      (concat "\\(" "^nn\\.a[0-9]+\\|\\.log\\|(ftp)"
              "\\)$"))

(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
(add-to-list 'desktop-modes-not-to-save 'DocView-mode)

;; allow midnight to nuke old buffers though the sessions
(add-to-list 'desktop-locals-to-save 'buffer-display-time)

(require 'midnight)
(cancel-timer midnight-timer)

;; nuke old buffer after running emacs
(run-with-idle-timer 10 nil
                     (lambda ()
                       (run-hooks 'midnight-hook)))

;; don't let Customize mess with my .emacs
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;; loading zenburn theme
(load-theme 'zenburn t)

;; BUG: require is cyan. Loading zenburn-theme.el fixes this
(load "zenburn-theme")

;; yasnippet
(add-to-list 'load-path "~/.emacs.d/vendor/yasnippet")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets")
(yas/global-mode 1)
(setq yas/triggers-in-field t)

;; Inter-field navigation
(defun yas/goto-end-of-active-field ()
  (interactive)
  (let* ((snippet (car (yas/snippets-at-point)))
         (position (yas/field-end (yas/snippet-active-field snippet))))
    (goto-char position)))

(defun yas/goto-start-of-active-field ()
  (interactive)
  (let* ((snippet (car (yas/snippets-at-point)))
         (position (yas/field-start (yas/snippet-active-field snippet))))
    (goto-char position)))

(define-key yas/keymap (kbd "C-e") 'yas/goto-end-of-active-field)
(define-key yas/keymap (kbd "C-a") 'yas/goto-start-of-active-field)

;; C-k in a field
(defun yas/clear-current-field ()
  (interactive)
  (let ((field (and yas/active-field-overlay
                    (overlay-buffer yas/active-field-overlay)
                    (overlay-get yas/active-field-overlay 'yas/field))))
    (and field (delete-region (point) (yas/field-end field)))))

(define-key yas/keymap (kbd "C-k") 'yas/clear-current-field)

;;; org-mode
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

(setq org-capture-templates
      '(
        ("t" "Todo" entry
         (file+headline "~/Dropbox/Org/someday.org" "Tâches")
         "* TODO %(todo-item)%?\n  OPENED: %U")
        ("d" "Téléchargement" entry
         (file+headline "~/Dropbox/Org/someday.org" "Downloads")
         "* TODO %?\n  OPENED: %U")
        ("m" "Emacs" entry
         (file+headline "~/Dropbox/Org/someday.org" "Programming")
         "* TODO %?                                                :emacs:\n  OPENED: %U")
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
         (file+headline "~/Dropbox/Org/specialdays.org" "")
         "* %^{Birthday}t Anniversaire de %^{prompt}!\n")
        ("b" "Boss" entry
         (file+headline "~/Dropbox/Org/someday.org" "Boss")
         "* TODO %?                                                :boss:\n  OPENED: %U")
        ("f" "Phone calls")
        ("fr" "Received phone calls" entry
         (file+headline "~/Dropbox/Org/phonecalls.org" "Received")
         "* Received phone call:\n  at %U\n  from %?")
        ("fm" "Made phone calls" entry
         (file+headline "~/Dropbox/Org/phonecalls.org" "Made")
         "* Made phone call:\n  at %U\n  to %?")
        ("l" "Livres empruntés")
        ("lu" "Bibliothèque Universitaire" entry
         (file+headline "~/Dropbox/Org/books.org" "Empruntés")
         "* BORROWED %?\n  BU Universitaire\n  BORROWED: %u\n  DEADLINE: %(deadline-from-now 28 4)")
        ("ll" "Bibliothèque du labo" entry
         (file+headline "~/Dropbox/Org/books.org" "Empruntés")
         "* BORROWED %?\n  BU Labo\n  BORROWED: %u\n  DEADLINE:  %(deadline-from-now 365 4)")))

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

(require 'org-bib-workflow)

;; bigger latex fragment
(plist-put org-format-latex-options :scale 1.5)


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

;; se rappelle ou je suis dans un fichier
(setq save-place-file "~/.emacs.d/saveplace") ;; keep my ~/ clean
(setq-default save-place t) ;; activate it for all buffers
(require 'saveplace) ;; get the package

;; retourne au dernier endroit changé dans le buffer
(require 'goto-last-change)
(global-set-key (kbd "C-x C-_") 'goto-last-change)

;; Indentation du buffer
(defun indent-buffer ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

;; if indent-tabs-mode is off, untabify before saving
(defun untabify-non-vc ()
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

(add-hook 'write-file-functions 'untabify-non-vc)

;; Numérotation des lignes dans la marge
(require 'linum)
(global-linum-mode 1)
(setq linum-format "%5d")

;; Correction orthographique
(setq ispell-dictionary "fr")
(require 'flyspell)

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

;;; Auctex
(add-to-list 'load-path "~/.emacs.d/auctex-cvs")
(add-to-list 'load-path "~/.emacs.d/auctex-cvs/preview")

(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

(setq auto-mode-alist (cons '("\\.tex$" . LaTeX-mode) auto-mode-alist))
(setq TeX-PDF-mode t)
(setq TeX-save-query nil) ; autosave before compiling

;; Needed to use external programs such as gnuplot
(setq LaTeX-command "latex --shell-escape")

;; indentation correcte des items
(setq LaTeX-item-indent 0)

;; disable fill in env
(eval-after-load "latex"
  '(mapc (lambda (env) (add-to-list 'LaTeX-indent-environment-list (list env)))
         '("tikzpicture" "scope" "figure")))

;; add subnumcases to the list of math environments
(eval-after-load "font-latex"
  '(add-to-list 'font-latex-math-environments "subnumcases"))

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (LaTeX-add-environments
             "equation*"
             '("subnumcases" "Before")
             '("block" "Title"))
            (when buffer-file-name
              (turn-on-reftex)
              (reftex-set-cite-format "~\\cite{%l}"))
            (auto-fill-mode)
            (flyspell-mode)
            (TeX-source-correlate-mode 1))) ; Source Specials
            ;;(add-to-list 'TeX-output-view-style '("^pdf$" "." "evince %o %(outpage)"))))

;; enable fr dictionary when using package frenchb
(add-hook 'TeX-language-fr-hook
           (lambda () (ispell-change-dictionary "fr")))

(setq TeX-view-program-list '(("Evince" "evince --page-label=%(outpage) %o")))


(defun latex-escape-or-unescape-accented-characters (&optional escape)
  "Escapes accented characters when no prefix argument. When
  escaping, the first element of a list is preferred when there
  is a list. When any prefix argument, unescape accented
  characters."
  (interactive "P")
  (let ((n 0)
        (beg (copy-marker
              (if (region-active-p) (region-beginning) (point-min))))
        (end (copy-marker
              (if (region-active-p) (region-end) (point-max)))))
    (save-excursion
      (mapc
       (lambda (e)
         (let* ((to (if escape (cdr e) (car e)))
                (to (if (listp to) (car to) to))
                (from (if escape (car e) (cdr e)))
                (from (if (listp from) from (list from))))
           (mapc
            (lambda (froma)
              (goto-char beg)
              (while (search-forward froma end t)
                (replace-match to nil t)
                (setq n (+ n 1))))
            from)))
       '((("\\'e" "\\'{e}") . "é")
         (("\\`e" "\\`{e}") . "è")
         (("\\`a" "\\`{a}") . "à")
         (("\\`u" "\\`{u}") . "ù")
         (("\\^e" "\\^{e}") . "ê")
         (("\\^o" "\\^{o}") . "ô")
         (("\\^u" "\\^{u}") . "û")
         (("\\\"i" "\\\"{i}") . "ï")
         ("\\c{c}" . "ç")
         (("\\^i" "\\^{i}") . "î"))))
    (message "Replaced %d occurences" n)))

;; bookmarks
(setq
 bookmark-default-file "~/.emacs.d/bookmarks" ;; keep my ~/ clean
 bookmark-save-flag 1)                        ;; autosave each change

;; gnome-open file that emacs can't
(defun gnome-open (filename)
  (let ((process-connection-type nil))
    (start-process "" nil "/usr/bin/gnome-open"
                   (expand-file-name filename))))

(defadvice find-file (around find-or-launch-file)
  "Gnome opens file that emacs can't."
  (cond
   ((string-match
     (concat
      "\\."
      (regexp-opt '("ods" "odt" "pdf") t)
      "$")
     (ad-get-arg 0))
    (gnome-open (ad-get-arg 0))
    (message "Gnome-opening file..."))
   (t
    ad-do-it)))

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

(load-file "~/Dropbox/scripts/auto-insert.el")

(setq auto-insert-alist
      '(
        ("\\.rb$"  "Ruby shebang" (auto-insert-yasnippet "shebang"))
        ("\\.sh$" "Bash shebang" (auto-insert-yasnippet "shebang"))
        ("\\.tex$"
         ("Latex article"
          (auto-insert-yasnippet "headerlatex"))
         ("Standalone TikZ"
          (auto-insert-yasnippet "headertikz"))
         ("Standalone TikZ for my thesis"
          (auto-insert-yasnippet "latextikzthesis"))
         ("Letter" (auto-insert-yasnippet "ll")))))


(setq auto-insert 'other)


(defun change-to-utf-8 ()
  (interactive)
  (set-buffer-file-coding-system 'utf-8-unix))


(global-set-key [(f2)] 'change-to-utf-8)

;;; ruby
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/inf-ruby"))
(autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process" t)
(autoload 'inf-ruby-keys "inf-ruby" "" t)
(eval-after-load 'ruby-mode
  '(progn
     (add-hook 'ruby-mode-hook 'inf-ruby-keys)
     (add-hook 'ruby-mode-hook
               (lambda ()
                 (define-key ruby-mode-map (kbd "RET")
                   'reindent-then-newline-and-indent)))))



(autoload 'yaml-mode "yaml-mode")
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))

;;; cmake-mode
(require 'cmake-mode)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                ("\\.cmake\\'" . cmake-mode))
              auto-mode-alist))

;;; shell-toggle
(autoload 'shell-toggle "shell-toggle"
  "Toggles between the shell buffer and whatever buffer you are editing."
  t)
(autoload 'shell-toggle-cd "shell-toggle"
  "Pops up a shell-buffer and insert a \"cd <file-dir>\" command." t)
(global-set-key [M-f1] 'shell-toggle)
(global-set-key [C-f1] 'shell-toggle-cd)

(setq shell-toggle-launch-shell 'shell-toggle-ansi-term)


;; minibuffer history
;;(savehist-mode t)

;; Always add a final newline
(setq require-final-newline t)

(defun delete-trailing-whitespace-non-vc ()
  "Delete trailing whitespace if file is not version controlled
or version controlled but untracked."
  (and
   (and (buffer-file-name)
        (or
         (not (vc-backend (buffer-file-name)))
         (eq (vc-backend (buffer-file-name)) 'none))
        (delete-trailing-whitespace))
   nil))

(add-hook 'write-file-functions 'delete-trailing-whitespace-non-vc)

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

;; don't warn when quitting emacs with running processes
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

;; don't warn when killing running processes
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; Make URLs in comments/strings clickable
(add-hook 'find-file-hooks 'goto-address-prog-mode)

;; tidy.el
(autoload 'tidy-buffer "tidy" "Run Tidy HTML parser on current buffer" t)
(autoload 'tidy-parse-config-file "tidy" "Parse the `tidy-config-file'" t)
(autoload 'tidy-save-settings "tidy" "Save settings to `tidy-config-file'" t)
(autoload 'tidy-build-menu  "tidy" "Install an options menu for HTML Tidy." t)

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

;; Customized Emacs Lisp mode
(require 'eldoc)
(autoload 'turn-on-eldoc-mode "eldoc" nil t)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)

(add-hook 'emacs-lisp-mode-hook
          (lambda()
            (setq mode-name "ELisp")
            (local-set-key (kbd "C-x E")
                           (lambda()(interactive)
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
            (setq lisp-indent-offset nil)
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
            (font-lock-add-keywords
             nil
             '(("^[^;\n]\\{80\\}\\(.*\\)$" 1 font-lock-warning-face prepend)))
            (font-lock-add-keywords
             nil
             '(("\\<\\(FIXME\\|TODO\\|BUG\\)"
                1 font-lock-warning-face prepend)))
            (font-lock-add-keywords
             nil
             '(("\\<\\(add-hook\\|setq\\)\\>"
                1 font-lock-keyword-face prepend)))))


(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(global-set-key (kbd "C-c e") 'eval-and-replace)

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

;; open bash-fc-* files from fc command or C-x C-e in terminal in sh-mode
(add-to-list 'auto-mode-alist '("bash-fc-[0-9]+\\'" . sh-mode))

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

(defun my-fill-paragraph (&optional arg)
  (interactive "P")
  (if arg
      (call-interactively 'unfill-paragraph)
    (call-interactively 'fill-paragraph)))

(defun unfill-paragraph ()
  "Replace newline chars in current paragraph by single spaces."
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph)))

(global-set-key (kbd "M-q") 'my-fill-paragraph)

;; Fonction équivalente à la précédente appliquée à la région sélectionnée et
;; non plus au paragraphe courant.
(defun remove-hard-wrap-region (start end)
  "Replace newline chars in region by single spaces."
  (interactive "r")
  (let ((fill-column most-positive-fixnum))
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


(defun my-reindent-then-newline-and-indent-and-indent-sexp ()
  "Reindent current line, insert newline, then indent the new line.
Move backward out of one level of parentheses.
Indent each line of the list starting just after point."
  (interactive "*")
  (reindent-then-newline-and-indent)
  (save-excursion
    (backward-up-list)
    (indent-sexp)))

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

;; (setq eval-expression-print-length 100)
;; (setq eval-expression-print-level 10)

;; This is sweet!  right-click, get a list of functions in the source
;; file you are editing
;; (http://emacs.wordpress.com/2007/01/24/imenu-with-a-workaround/#comment-51)
(global-set-key [mouse-3] `imenu)


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
;;      (and (not (equal he-search-string ""))
;;        (mapcar (function (lambda (sym)
;;                            (if (and (boundp sym) (vectorp (eval sym)))
;;                              (abbrev-expansion (downcase he-search-string)
;;                                (eval sym)))))
;;          (append '(local-abbrev-table
;;                     global-abbrev-table)
;;            abbrev-table-name-list))))))
;;   (while (and he-expand-list
;;         (or (not (car he-expand-list))
;;           (he-string-member (car he-expand-list) he-tried-table t)))
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
;;       (accept-process-output ispell-process)
;;       (not (string= "" (car ispell-filter)))))
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


(defun pcomplete-erc-command-name ()
  "Returns the command name of the first argument."
  (let ((cmd (pcomplete-arg 'first)))
    (cond
     ((member (substring cmd 0 -1)
              (pcomplete-erc-nicks))
      "NICKLIST")
     ((eq (elt cmd 0) ?/)
      (upcase (substring cmd 1)))
     (t "SAY"))))

(defun is-nick-p (nick)
  (member (substring nick 0 -1)
          (pcomplete-erc-nicks)))

(defun pcomplete/erc-mode/NICKLIST ()
  (while (and (pcomplete-test 'is-nick-p)
              (or (= pcomplete-index pcomplete-last) (pcomplete-test 'is-nick-p 0)))
    (let ((start erc-input-marker))
      (save-excursion
        (goto-char (pcomplete-begin 0))
        (while (re-search-backward ": " start t)
          (replace-match ", "))))
    (pcomplete-here (pcomplete-erc-nicks ": ")))
  (while (pcomplete-here (pcomplete-erc-nicks))))

(require 'epa)
(epa-file-enable)

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

(add-to-list  'load-path "~/.emacs.d/vendor/google-weather-el")
(setq url-cache-directory "~/.emacs.d/cache")
(require 'org-google-weather)

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


(defun latex-delete-unreferenced-labels ()
  "Delete all occurences of a label that is not referenced in the
document."
  (interactive)
  (save-excursion
    (let (labels (count 0))
      (goto-char (point-min))
      (while (re-search-forward "\\\\\\(eq\\|page\\|[fvF]\\)?ref{\\([^\n\r%\\{}]+\\)}" nil t)
        (setq labels (cons (match-string-no-properties 1) labels)))
      (goto-char (point-min))
      (while (re-search-forward "\\\\label{\\([^\n\r%\\{}]+\\)}" nil t)
        (unless (member (match-string-no-properties 1) labels)
          (delete-region (match-beginning 0) (match-end 0))
          (setq count (+ 1 count))))
      (message "%s label%s deleted!"
               (if (= count 0) "No" (int-to-string count))
               (if (>= count 2) "s" "")))))


(defun enclosing-braces-at-point ()
  (and (thing-at-point-looking-at "{\\([^}]*\\)}")
       (buffer-substring-no-properties (match-beginning 1) (match-end 1))))

(defun latex-refactor-label (label new)
  "Rename a label and its references in a LaTeX document. Word at
point is suggested as the default label to replace. A message
shows you how many labels and refs have been replaced."
  (interactive
   (list (let ((tap (or (enclosing-braces-at-point) (thing-at-point 'word))))
           (read-string
            (format "Old label%s: " (if tap (concat " (" tap ")") ""))
            nil nil tap))
         (read-string "New label: ")))
  (save-excursion
    (save-restriction
      (and mark-active (narrow-to-region (region-beginning) (region-end)))
      (message
       (concat "\"%s\" -> \"%s\": "
               (mapconcat
                (lambda (args)
                  (goto-char (point-min))
                  (let ((n 0))
                    (while (re-search-forward
                            (concat "\\\\\\(?:" (car args) "\\){\\("
                                    (regexp-quote label) "\\)}") nil t)
                      (setq n (1+ n))
                      (replace-match new t t nil 1))
                    (format "%d %s%s" n (cdr args) (if (> n 1) "s" ""))))
                '(("label" . "label")
                  ("\\(?:eq\\|page\\|[fvF]\\)?ref" . "reference"))
                " and ")
               " replaced in %s!")
       label new (if mark-active "region" "buffer")))))

(defun latex-occur-ref-wo-tilde ()
  (interactive)
  (occur "[^~(](?\\\\\\(eq\\|page\\|[fvF]\\)?ref"))

(defun latex-occur-ref-with-parent ()
  (interactive)
  (occur "(\\\\ref{[^{]*})"))

(global-set-key [(control tab)] 'other-window)

(add-to-list 'load-path "~/.emacs.d/vendor/org-mode/contrib/lisp")
(require 'org-drill)

;; trying paredit
(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code."
  t)

;; trying expand-region
(add-to-list 'load-path "~/.emacs.d/vendor/expand-region.el")
(require 'expand-region)
(global-set-key (kbd "C-à") 'er/expand-region)
(global-set-key (kbd "C-M-à") 'er/contract-region)

(global-set-key (kbd "C-j")
                (lambda ()
                  (interactive)
                  (kill-line 0)))
