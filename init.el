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

(add-to-list 'load-path (concat dotfiles-dir "site-lisp"))
(dolist (path (directory-files
               (concat dotfiles-dir "site-lisp") t "[^\\.]\\|\\(\\.\\{3,\\}\\)"))
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
          (nreverse form-list)))))

(defun require-maybe (feat)
  "Like `require' but display a message instead of signaling an error."
  (or (require feat nil t)
      (not (message "Feature `%s' not loaded!" feat))))

;; adding packages source
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t)

  (package-initialize)

  (setq my-packages '(auctex yari twittering-mode diminish elisp-slime-nav))

  (when (memq nil (mapcar 'package-installed-p my-packages))
    (message "Refreshing packages database...")
    (package-refresh-contents)
    (dolist (p my-packages)
      (when (not (package-installed-p p))
        (package-install p)))))

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
(require 'init-scratch)
(require 'init-twittering)
(require 'init-hippie-expand)
(require 'init-vanilla)

;; whitespace mode
(require 'whitespace)

(setq whitespace-style
      '(face trailing tabs))

(global-whitespace-mode)

(load-library "paren")
(show-paren-mode 1)

(require 'epa)
(epa-file-enable)

(require 'expand-region)
(global-set-key (kbd "C-à") 'er/expand-region)
(global-set-key (kbd "C-M-à") 'er/contract-region)

;; fast navigation from symbol to definition
(when (require-maybe 'elisp-slime-nav)
  (add-hook 'emacs-lisp-mode-hook
            (lambda () (elisp-slime-nav-mode t))))

(when (require-maybe 'diminish)
  (eval-after-load "undo-tree"
    '(diminish 'undo-tree-mode))
  (eval-after-load 'elisp-slime-nav
    '(diminish 'elisp-slime-nav-mode)))

(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)
(setq browse-kill-ring-quit-action 'save-and-restore)

;; line numbering
(require 'linum)
(global-linum-mode 1)
(setq linum-format "%5d")

(autoload 'yaml-mode "yaml-mode")
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))

;; Minor mode to resolve diff3 conflicts
(autoload 'smerge-mode "smerge-mode" nil t)

(defun sm-try-smerge ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " nil t)
      (smerge-mode 1))))
(add-hook 'find-file-hook 'sm-try-smerge t)

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
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))

;; flymake for shell scripts
(require 'flymake-shell)
(add-hook 'sh-set-shell-hook 'flymake-shell-load)

;; tidy.el
(autoload 'tidy-buffer "tidy" "Run Tidy HTML parser on current buffer" t)
(autoload 'tidy-parse-config-file "tidy" "Parse the `tidy-config-file'" t)
(autoload 'tidy-save-settings "tidy" "Save settings to `tidy-config-file'" t)
(autoload 'tidy-build-menu  "tidy" "Install an options menu for HTML Tidy." t)

;; ace-jump-mode
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(set-face-foreground 'ace-jump-face-foreground "yellow")

;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode)

(require 'mwheel)
(mouse-wheel-mode 1)

;; Make zooming affect frame instead of buffers
(require 'zoom-frm)
(global-set-key (if (boundp 'mouse-wheel-down-event) ; Emacs 22+
                    (vector (list 'control mouse-wheel-down-event))
                  [C-mouse-wheel])      ; Emacs 20, 21
                'zoom-in)
(when (boundp 'mouse-wheel-up-event)    ; Emacs 22+
  (global-set-key (vector (list 'control mouse-wheel-up-event))
                  'zoom-out))

;; wtf for acronym lookup
(require 'wtf)

;; buffers can't have the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets
      uniquify-after-kill-buffer-p t)

(require 'saveplace)
(setq save-place-file "~/.emacs.d/.saveplace")
(setq-default save-place t)

;; goto last changed place in buffer
(require 'goto-last-change)
(global-set-key (kbd "C-x C-_") 'goto-last-change)

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

;; From https://github.com/jwiegley/dot-emacs
(defun recentf-add-dired-directory ()
  (if (and dired-directory
           (file-directory-p dired-directory)
           (not (string= "/" dired-directory)))
      (let ((last-idx (1- (length dired-directory))))
        (recentf-add-file
         (if (= ?/ (aref dired-directory last-idx))
             (substring dired-directory 0 last-idx)
           dired-directory)))))

(add-hook 'dired-mode-hook 'recentf-add-dired-directory)

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

;;; sh-toggle
(require 'sh-toggle)
(setq shell-toggle-shell 'ansi-term)

(global-set-key (kbd "C-z") 'shell-toggle)
(global-set-key (kbd "C-M-z") 'shell-toggle-cd)

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

;; loading zenburn theme
(load-theme 'zenburn t)

;; BUG: require is cyan. Loading zenburn-theme.el fixes this
(load "zenburn-theme")

;; paste in term
(require 'term)

(add-hook 'term-mode-hook
          (lambda ()
            (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
            (make-local-variable 'mouse-yank-at-point)
            (make-local-variable 'transient-mark-mode)
            (setq mouse-yank-at-point t)
            (setq transient-mark-mode nil)
            (auto-fill-mode -1)
            (setq tab-width 8)
            (setq truncate-lines t)
            (define-key term-raw-map (kbd "C-y") 'term-paste)
            (define-key term-raw-map (kbd "C-z") 'shell-toggle)))

;; notify events
(when (>= emacs-major-version 24)
  (require 'notifications))

(require 'ffap)
;; find file at point even if it is the wrong user: /home/otheruser/realfile
(add-to-list 'ffap-alist
             (cons "\\`\\(/home/[^/]+\\)"
                   (lambda (name)
                     (replace-match "~" nil nil name 1))))

;; find pdf at a ref which has the same name in `pdfs-directory'
(defvar pdfs-directory nil
  "Directory to look for pdf files.")

(put 'pdfs-directory 'safe-local-variable 'string-or-null-p)

(defun ffap-bib-latex-mode (name)
  (and pdfs-directory (concat pdfs-directory name ".pdf")))

(add-to-list 'ffap-alist '(latex-mode . ffap-bib-latex-mode))
(add-to-list 'ffap-alist '(org-mode . ffap-bib-latex-mode))

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

(require-maybe 'vc-git-check-status)
(require-maybe 'org-context)
(require-maybe 'commit-message)

;; load file settings if any
(ignore-errors (load-file "~/Dropbox/emacs/org-context-settings.el"))

(require-maybe 'vc-git-commit-all)

(require-maybe 'init-autoinsert)

(require-maybe 'org-bib-workflow)

(require-maybe 'find-temp-file)

(unless (server-running-p)
  (server-start))

(add-to-list 'default-frame-alist
             '(font . "-unknown-Inconsolata-bold-normal-normal-*-*-*-*-*-m-0-iso10646-1"))

;; disable dialog box, tool bar...
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(tool-bar-mode -1)
(setq inhibit-startup-screen t)

;; UTF-8 as default encoding
(set-language-environment "UTF-8")

;; unified diff format and no whitespace when using `diff'
(setq diff-switches "-u -w")

;; no limit on how many lines to keep in *Messages* buffer
(setq message-log-max t)

;; no fringe on the right
(set-fringe-mode '(8 . 0))

;; no fringe in minibuffer
(set-window-fringes (minibuffer-window) 0 0)

;; quit minibuffer if there is a click on another buffer
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

;; suit les liens vers les systèmes de contrôle de versions
(setq vc-follow-symlinks t)

(defalias 'yes-or-no-p 'y-or-n-p)

;; leave point at center of the screen when scrolling
(setq scroll-preserve-screen-position t)

;; SHIFT selection
;;(custom-set-variables '(pc-selection-mode t nil (pc-select)))

;; .dir-locals.el helper
(defun dir-locals-get-directory (file)
  "Return the directory containing the directory local file that
is read by the file FILE.
Useful if you want to set parameters depending on the location of
that directory local file."
  (let ((dir (dir-locals-find-file file)))
    (if (listp dir)
        (car dir)
      (file-name-directory dir))))

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

;; auto-indent pasted code
(dolist (func '(yank yank-pop))
  (ad-add-advice
   func
   `(,(intern (format "%s-advice" func)) nil t
     (advice . (lambda ()
                 "Auto indent on paste"
                 (maybe-indent-on-paste))))
   'after
   'last)
  (ad-activate func))

(defun maybe-indent-on-paste ()
  (if (or (derived-mode-p 'prog-mode)
          (memq major-mode '(ruby-mode
                             emacs-lisp-mode scheme-mode
                             lisp-interaction-mode sh-mode
                             lisp-mode c-mode c++-mode objc-mode
                             latex-mode plain-tex-mode
                             python-mode matlab-mode)))
      (indent-region (region-beginning) (region-end))))

(defun kill-region-or-backward ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (kill-line 0)))

(global-set-key (kbd "C-w") 'kill-region-or-backward)

(defun save-region-or-current-line ()
  (interactive)
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (kill-ring-save (line-beginning-position) (line-beginning-position 2))
    (message "line copied")))

(global-set-key (kbd "M-w") 'save-region-or-current-line)

(global-set-key (kbd "M-j") (lambda () (interactive) (join-line t)))

;; copy when in read only buffer
(setq kill-read-only-ok t)

(defun beginning-of-line-or-text ()
  (interactive)
  (if (bolp)
      (beginning-of-line-text)
    (beginning-of-line)))

(global-set-key (kbd "C-a") 'beginning-of-line-or-text)

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

;; C-d to kill buffer if process is dead.
(defun comint-delchar-or-eof-or-kill-buffer (arg)
  (interactive "p")
  (if (null (get-buffer-process (current-buffer)))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))

(add-hook 'shell-mode-hook
          (lambda ()
            (define-key shell-mode-map (kbd "C-d")
              'comint-delchar-or-eof-or-kill-buffer)))

;; echo keystrokes quickly
(setq echo-keystrokes 0.1)

;; delete all whitespace when deleting backward
(setq backward-delete-char-untabify-method 'all)

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

;; no fast scrolling
(setq mouse-wheel-progressive-speed nil)

;; backups
(setq make-backup-files t ;; do make backups
      ;;  backup-by-copying t     ;; and copy them here
      backup-directory-alist '((".*" . "~/.emacs.d/emacs.backups"))
      version-control t
      kept-new-versions 2
      kept-old-versions 5
      delete-old-versions t)

;; don't backup sudo opened files and .recentf
(defun my-dont-backup-files-p (filename)
  (unless (or
           (string-match "\\`/sudo:root@" filename)
           (string-match "\\.recentf$" filename))
    (normal-backup-enable-predicate filename)))

(setq backup-enable-predicate 'my-dont-backup-files-p)

(setq auto-save-list-file-prefix
      "~/.emacs.d/cache/auto-save-list/.saves-")
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; don't let Customize mess with my .emacs
(setq custom-file (concat site-lisp-dir "custom.el"))
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
  "Delete trailing whitespaces if file is not version controlled
or version controlled but untracked. Make sure to return `nil' in
case it is used in hooks."
  (and (buffer-file-name)
       (or
        (memq (vc-backend (buffer-file-name)) '(nil none))
        ;; git backend and autocommitted repo
        (and (memq (vc-backend (buffer-file-name)) vc-handled-backends)
             (vc-git-auto-committed-p)))
       (delete-trailing-whitespace)
       nil))

;; if indent-tabs-mode is off, untabify before saving
(defun untabify-safe ()
  "Untabify buffer if it is not VC or untracked in VC and if
`indent-tabs-mode' is nil."
  (and (buffer-file-name)
       (or
        (memq (vc-backend (buffer-file-name)) '(nil none))
        (and (memq (vc-backend (buffer-file-name)) vc-handled-backends)
             (and (fboundp 'vc-git-auto-committed-repo-p)
                  (vc-git-auto-committed-p))))
       (not indent-tabs-mode)
       (untabify (point-min) (point-max))
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
(setq bookmark-default-file
      (if (file-exists-p "~/Dropbox/emacs/.bookmarks")
          "~/Dropbox/emacs/.bookmarks"
        "~/.emacs.d/bookmarks"))
(setq bookmark-save-flag 1)

(defun ring-transparency (arg)
  "Selects next transparency setting. When used with
\\[universal-argument] jumps between first and last setting."
  (interactive "P")
  (let* ((ring '(100 50 25 0))
         (current (or (frame-parameter nil 'alpha) (car ring)))
         (last (car (last ring)))
         (next (if arg
                   (if (equal current (car ring)) last (car ring))
                 (or (cadr (member current ring)) (car ring)))))
    (set-frame-parameter nil 'alpha next)))

;; ignore case when completing
(setq completion-ignore-case t)

;; filenames too, to browse with dired for example...
(setq read-file-name-completion-ignore-case t)

(defun change-to-utf-8 ()
  (interactive)
  (set-buffer-file-coding-system 'utf-8-unix))

(global-set-key [(f2)] 'change-to-utf-8)

;; minibuffer history
;;(savehist-mode t)

;; Always add a final newline
(setq require-final-newline t)

;; Display time in modeline
(display-time)
(setq display-time-24hr-format 1)

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

;; activate automatic timestamp
(setq
 time-stamp-active t
 time-stamp-format "%04y-%02m-%02d %02H:%02M:%02S (%u)") ; date format

(add-hook 'before-save-hook 'time-stamp)

;; Make scripts executable on save
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; open bash-fc-* files from fc command or C-x C-e in terminal in sh-mode
(add-to-list 'auto-mode-alist '("bash-fc-[0-9]+\\'" . sh-mode))

(setq truncate-lines nil)
(setq truncate-partial-width-windows nil)

(defmacro debug-print (obj)
  (let ((obsym (make-symbol "object")))
    `(let ((,obsym ,obj))
       (unless (get-buffer "*debug*")
         (with-current-buffer (get-buffer-create "*debug*")
           (emacs-lisp-mode)))
       (with-current-buffer  (get-buffer "*debug*")
         (goto-char (point-max))
         (or (bolp) (newline))
         (insert (format ";; [%s]\n%s" (current-time-string)
                         (pp-to-string ,obsym)))
         (if (get-buffer-window "*debug*")
             (set-window-point (get-buffer-window "*debug*") (point))))
       ,obsym)))

;; From http://www.emacswiki.org/emacs/EmacsAsDaemon
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

;; From http://www.dotemacs.de/dotfiles/BenjaminRutt.emacs.html
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
all cases (even in an emacs -nw session)."
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

;;; From http://emacs-journey.blogspot.fr/2012/06/re-builder-query-replace-this.html
(defun reb-query-replace-this-regxp (replace)
  "Uses the regexp built with re-builder to query the target buffer.
This function must be run from within the re-builder buffer, not the target
buffer.

Argument REPLACE String used to replace the matched strings in the buffer.
 Subexpression references can be used (\1, \2, etc)."
  (interactive "sReplace with: ")
  (let* ((reg (reb-read-regexp))
         (newcmd (list 'query-replace-regexp reg replace)))
    (select-window reb-target-window)
    (save-excursion
      (goto-char (point-min))
      (or (equal newcmd (car command-history))
          (setq command-history (cons newcmd command-history)))
      (eval newcmd)))
  (reb-quit))

(eval-after-load "re-builder"
  '(define-key reb-mode-map "\C-c\M-%" 'reb-query-replace-this-regxp))
