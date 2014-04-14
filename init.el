(load "~/Dropbox/emacs/personnal.el" :noerror)

(require 'init-utils (expand-file-name "init-utils" user-emacs-directory))

;; Add personnal site-lisp to load-path
(defvar site-lisp-directory "~/Dropbox/emacs/site-lisp/")

(when (file-exists-p site-lisp-directory)
  (add-to-list 'load-path site-lisp-directory)
  (add-subdirs-to-load-path site-lisp-directory))

;; Add .emacs.d/site-lisp to load path and all sub-directories
(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))

(add-subdirs-to-load-path (concat user-emacs-directory "site-lisp"))

(define-on-macro "knuth")

;; Disable dialog box, tool bar...
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(menu-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode)
(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)

;; Backups
(setq make-backup-files t ;; do make backups
      ;;  backup-by-copying t     ;; and copy them here
      backup-directory-alist '((".*" . "~/.emacs.d/emacs.backups"))
      version-control t
      kept-new-versions 2
      kept-old-versions 5
      delete-old-versions t)

;; Don't backup sudo opened files and .recentf
(defun my-dont-backup-files-p (filename)
  (unless (or
           (string-match "\\`/sudo:root@" filename)
           (string-match "\\.recentf$" filename)
           (string-match "\\.gpg$" filename))
    (normal-backup-enable-predicate filename)))

(setq backup-enable-predicate 'my-dont-backup-files-p)

(setq auto-save-list-file-prefix
      "~/.emacs.d/cache/auto-save-list/.saves-")
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Adding packages
(with-emacs-version>= "24"
  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t)

  (package-initialize)

  (defvar package-required-packages
    '(async
      auctex
      dash
      diminish
      elisp-slime-nav
      google-translate
      helm-descbinds
      legalese
      lua-mode
      macrostep
      multi-term
      multiple-cursors
      offlineimap
      projectile
      s
      twittering-mode
      wcheck-mode
      yaml-mode
      yari
      zenburn-theme)
    "List of required packages")

  (catch 'timeout
    (when (memq nil (mapcar 'package-installed-p package-required-packages))
      (message "Refreshing packages database...")
      (with-timeout (10 (message "Timeout, cancelling...")
                        (sit-for 2)
                        (throw 'timeout nil))
        (package-refresh-contents))
      (mapc (lambda (p)
              (when (not (package-installed-p p))
                (package-install p)))
            package-required-packages))))

;; Set path as if emacs were run in a terminal
(let ((path (shell-command-to-string "bash -i -c 'echo $PATH' 2> /dev/null")))
  (setenv "PATH" path)
  (setq exec-path (split-string path "[:\n]" t)))

;; Loading zenburn theme
(load-theme 'zenburn t)

(require 'init-fill (expand-file-name "init-fill" user-emacs-directory))
(require 'init-dired (expand-file-name "init-dired" user-emacs-directory))
(require 'init-isearch (expand-file-name "init-isearch" user-emacs-directory))
(require 'init-boss-key (expand-file-name "init-boss-key" user-emacs-directory))
(require 'init-erc (expand-file-name "init-erc" user-emacs-directory))
(require 'init-magit (expand-file-name "init-magit" user-emacs-directory))
(require 'init-find-file (expand-file-name "init-find-file" user-emacs-directory))
(require 'init-latex (expand-file-name "init-latex" user-emacs-directory))
(require 'init-desktop (expand-file-name "init-desktop" user-emacs-directory))
(require 'init-midnight (expand-file-name "init-midnight" user-emacs-directory))
(require 'init-helm (expand-file-name "init-helm" user-emacs-directory))
(require 'init-octave (expand-file-name "init-octave" user-emacs-directory))
(require 'init-matlab (expand-file-name "init-matlab" user-emacs-directory))
(require 'init-yasnippet (expand-file-name "init-yasnippet" user-emacs-directory))
(require 'init-org (expand-file-name "init-org" user-emacs-directory))
(require 'init-elisp (expand-file-name "init-elisp" user-emacs-directory))
(require 'init-auctex (expand-file-name "init-auctex" user-emacs-directory))
(require 'init-ibuffer (expand-file-name "init-ibuffer" user-emacs-directory))
(require 'init-bindings (expand-file-name "init-bindings" user-emacs-directory))
(require 'init-scratch (expand-file-name "init-scratch" user-emacs-directory))
(require 'init-twittering (expand-file-name "init-twittering" user-emacs-directory))
(require 'init-hippie-expand (expand-file-name "init-hippie-expand" user-emacs-directory))
(require 'init-vanilla (expand-file-name "init-vanilla" user-emacs-directory))
(require 'init-editing (expand-file-name "init-editing" user-emacs-directory))
(require 'init-paredit (expand-file-name "init-paredit" user-emacs-directory))
(require 'init-wcheck (expand-file-name "init-wcheck" user-emacs-directory))
(require 'init-ruby (expand-file-name "init-ruby" user-emacs-directory))

;; Whitespace mode
(require 'whitespace)

(setq whitespace-style
      '(face trailing tabs))

(global-whitespace-mode)

(load-library "paren")
(show-paren-mode 1)

(with-emacs-version>= "24.1"
  (electric-indent-mode 1))

(require 'webjump)
(global-set-key "\C-cj" 'webjump)

(require 'wgrep)

;; (add-to-list 'load-path "~/.emacs.d/site-lisp/gnus/lisp")
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/gnus/contrib")
;; (require 'gnus-load)

;; (on-knuth
;;  (require 'offlineimap)
;;  (add-hook 'gnus-before-startup-hook 'offlineimap))

;; (require 'bbdb-loaddefs "~/.emacs.d/site-lisp/bbdb/lisp/bbdb-loaddefs.el")
;; (bbdb-initialize 'gnus)
;; (setq bbdb-mua-auto-update-init 'gnus)
;; (setq bbdb-file "~/Dropbox/emacs/.bbdb")
;; (setq bbdb-message-all-addresses t)

;; ;; BUG gnus-timer--function undefined
;; (require 'gnus-util)

(with-emacs-version< "24"
  (require 'epa)
  (epa-file-enable))

(require 'expand-region)
(global-set-key (kbd "C-à") 'er/expand-region)
(global-set-key (kbd "C-M-à") 'er/contract-region)

(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;; Fast navigation from symbol to definition
(when (require-maybe 'elisp-slime-nav)
  (add-hook 'emacs-lisp-mode-hook
            (lambda () (elisp-slime-nav-mode t))))

(when (require-maybe 'diminish)
  (eval-after-load "undo-tree"
    '(diminish 'undo-tree-mode))
  (eval-after-load 'elisp-slime-nav
    '(diminish 'elisp-slime-nav-mode)))

;; Autoload rainbow-mode
(autoload 'rainbow-mode "rainbow-mode"
  "rainbow-mode autoload" t)

(autoload 'yaml-mode "yaml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))

;; Minor mode to resolve diff3 conflicts
(autoload 'smerge-mode "smerge-mode" nil t)

(defun sm-try-smerge ()
  (let ((old-point (point)))
    (goto-char (point-min))
    (if (re-search-forward "^<<<<<<< " nil t)
        (smerge-mode 1)
      (goto-char old-point))))

(add-hook 'find-file-hook 'sm-try-smerge t)

;; On-the-fly checker
(require 'flycheck)
(global-flycheck-mode 1)
(mapc (lambda (checker)
        (delq checker flycheck-checkers))
      '(emacs-lisp emacs-lisp-checkdoc tex-chktex tex-lacheck))

;;; cmake-mode
(require 'cmake-mode)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                ("\\.cmake\\'" . cmake-mode))
              auto-mode-alist))

;;; google translate
(require 'google-translate)
(global-set-key "\C-ct" 'google-translate-at-point)
(global-set-key "\C-cT" 'google-translate-query-translate)

(setq google-translate-default-source-language "en")
(setq google-translate-default-target-language "fr")

;; Projectile
(projectile-global-mode)
(eval-after-load 'projectile
  '(progn
     (setq projectile-known-projects-file (expand-file-name "cache" user-emacs-directory))
     (setq projectile-cache-file (expand-file-name "cache/projectile.cache" user-emacs-directory))
     (defun projectile-find-file-other-window (arg)
       "Jump to a project's file using completion.

With a prefix ARG invalidates the cache first."
       (interactive "P")
       (when arg
         (projectile-invalidate-cache nil))
       (let ((file (projectile-completing-read "Find file: "
                                               (projectile-current-project-files)))
             (root (projectile-project-root)))
         (other-window 1)
         (find-file (expand-file-name file root))
         (run-hooks 'projectile-find-file-hook)))

     (define-key projectile-mode-map
       (concat projectile-keymap-prefix (kbd "v"))
       'projectile-find-file-other-window)))

;; Markdown
(autoload 'markdown-mode "markdown-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))

;; Tidy
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
(define-key undo-tree-visualizer-mode-map (kbd "RET")
  'undo-tree-visualizer-quit)

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

(require 'transpose-frame)

;; wtf for acronym lookup
(require 'wtf)

;; Buffers can't have the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets
      uniquify-after-kill-buffer-p t)

(require 'saveplace)
(setq save-place-file "~/.emacs.d/cache/.saveplace")
(setq-default save-place t)

;; Goto last changed place in buffer
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
        "*helm for files*"
        "*helm find-file*"
        "*helm complete*"
        "*Ibuffer*"
        "*Calendar*"
        "*helm*"))
(global-set-key (kbd "M-N") 'winner-redo)
(global-set-key (kbd "M-P") 'winner-undo)


;;; sh-toggle
(require 'sh-toggle)
(setq shell-toggle-shell 'ansi-term)

(global-set-key (kbd "C-z") 'shell-toggle)
(global-set-key (kbd "C-M-z") 'shell-toggle-cd)

(defun summon-tmux ()
  (interactive)
  (let ((command
         "urxvt -T my-tmux -e bash -c \"tmux -q has-session && exec tmux attach-session -d || exec tmux new-session -n$USER -s$USER@$HOSTNAME\"; sleep 0.5; wmctrl -a my-tmux &"))
    (start-process "Shell" nil "bash" "-c" command)))

(global-set-key (kbd "C-z") 'summon-tmux)


;; Additional menu
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

;; Paste in term
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

;; Notify events
(with-emacs-version>= "24"
  (require 'notifications))

(require 'ffap)
;; Find file at point even if it is the wrong user: /home/otheruser/realfile
(add-to-list 'ffap-alist
             (cons "\\`\\(/home/[^/]+\\)"
                   (lambda (name)
                     (replace-match "~" nil nil name 1))))

;; Find pdf at a ref which has the same name in `pdfs-directory'
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

(require-maybe 'vc-check-status)

(add-to-list 'vc-check-cancel-hook
             (lambda ()
               (and
                (fboundp 'vc-auto-commit-backend)
                (vc-auto-commit-backend))))

(vc-check-status-activate)

(require-maybe 'org-context)
(org-context-activate)

;; (require 'scratch-message)
;; (scratch-message-toggle-activate 1)

;; (defun scratch-message-contrepet ()
;;   (require 'fortune)
;;   (with-demoted-errors "Error: %S"
;;       (fortune-in-buffer t "~/.conky/contrepétries")
;;     (scratch-message-insert
;;      (with-current-buffer fortune-buffer-name
;;        (buffer-string)))))

;; (setq scratch-message-function 'scratch-message-contrepet)

(require-maybe 'commit-message)

(when (require-maybe 'vc-auto-commit)
  (vc-auto-commit-activate)
  (global-set-key (kbd "C-x v C") 'vc-auto-commit))

(when (require-maybe 'autoinsert)
  ;; Using modified version of autoinsert to allow multiple autoinsert
  ;; https://github.com/thisirs/auto-insert-multiple.git

  ;; Adds hook to find-files-hook
  (auto-insert-mode t)

  (setq auto-insert-directory (expand-file-name "~/.emacs.d/autoinsert/"))
  (setq auto-insert-query nil)

  (setq auto-insert-alist
        '(
          ("\\.rb$"  "Ruby shebang" (auto-insert-yasnippet "sb"))
          ("\\.sh$" "Bash shebang" (auto-insert-yasnippet "sb"))
          ("\\.tex$"
           ("Latex article"
            (progn
              (auto-insert-yasnippet "hdr")
              (TeX-normal-mode 1)))
           ("Standalone TikZ"
            (progn
              (auto-insert-yasnippet "hdrt")
              (TeX-normal-mode 1)))
           ("Minimal LaTeX snippet"
            (progn
              (auto-insert-yasnippet "hdrm")
              (TeX-normal-mode 1)))
           ("Letter"
            (progn
              (auto-insert-yasnippet "hdrl")
              (TeX-normal-mode 1))))))

  (setq auto-insert 'other))

(require-maybe 'org-bib-workflow)

(when (require-maybe 'find-temp-file)
  (setq find-temp-file-directory "~/deathrow/drafts/")
  (setq find-temp-template-default "%M/%D/%N-%S.%E")
  (global-set-key (kbd "C-x C-t") 'find-temp-file))

(when (require-maybe 'state)
  (setq state-alist
        '((personnal
           (key . "p")
           (switch . "~/Dropbox/Org/personnel.org.gpg"))
          (boss
           (key . "b")
           (state-p . (string= "TAGS boss" (or org-agenda-name "")))
           (switch . (wconf-fullscreen 'org (org-agenda nil "b"))))
          (debug
           (key . "d")
           (switch . "*debug*"))
          (shell
           (key . "z")
           (switch . (unless (get-buffer "*eshell*")
                       (eshell)))
           (state-p . (equal (buffer-name) "*eshell*")))
          (emacs
           (key . "e")
           (switch . "~/.emacs.d/init.el"))
          (gnus
           (key . "g")
           (state-p . (memq major-mode
                            '(message-mode
                              gnus-group-mode
                              gnus-summary-mode
                              gnus-article-mode)))
           (switch . (wconf-fullscreen 'gnus (gnus))))
          (erc
           (key . "i")
           (state-p . (memq (current-buffer)
                            (erc-buffer-list)))
           (switch . (erc-start-or-switch 1)))
          (message
           (key . "m")
           (switch . "*Messages*"))
          (scratch
           (key . "s")
           (switch . "*scratch*"))
          (twit
           (key . "t")
           (state-p . (and (fboundp 'twittering-mode) (twittering-buffer-p)))
           (switch . (wconf-fullscreen 'twit (twit))))
          (org
           (key . "a")
           (state-p . (string= "*Org Agenda(t)*" (or (buffer-name) "")))
           (switch . (wconf-fullscreen 'org (org-agenda nil "t"))))))
  (state-install-bindings))

(require-maybe 'helm-bib)

(unless (server-running-p)
  (server-start))

(add-to-list 'default-frame-alist
             '(font . "-unknown-Inconsolata-bold-normal-normal-*-*-*-*-*-m-0-iso10646-1"))

;; No lockfiles
(with-emacs-version>= "24.2"
  (setq create-lockfiles nil))

;; UTF-8 as default encoding
(set-language-environment "UTF-8")

;; Unified diff format and no whitespace when using `diff'
(setq diff-switches "-u -w")

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; No limit on how many lines to keep in *Messages* buffer
(setq message-log-max t)

;; No fringe on the right
(set-fringe-mode '(8 . 0))

;; No fringe in minibuffer
(set-window-fringes (minibuffer-window) 0 0)

;; Don't automatically split vertically
(setq split-height-threshold nil)

;; Quit minibuffer if there is a click on another buffer
(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)


;; C-v when reading a file name in minibuffer go to root
(defun vc-responsible-backend-root (file)
  "Return ROOT if file is under a version controlled system. If
not, return nil."
  (catch 'found
    (dolist (backend vc-handled-backends)
      (let ((path (vc-call-backend backend 'responsible-p file)))
        (if path (throw 'found path))))))

(defun minibuffer--goto-root ()
  (interactive)
  (let* ((path (vc-responsible-backend-root
                (buffer-substring (field-beginning)
                                  (field-end)))))
    (when path
      (delete-minibuffer-contents)
      (insert path))))

(define-key minibuffer-local-filename-completion-map (kbd "C-v")
  'minibuffer--goto-root)

;; No fast scrolling
(setq mouse-wheel-progressive-speed nil)

;; Drive out the mouse when it's too near to the cursor.
(mouse-avoidance-mode 'animate)

(defun update-locate-database (directory)
  "Update locate databases"
  (interactive)
  (and (file-exists-p directory)
       (set-process-sentinel
        (start-process-shell-command
         "updatedb process" nil
         (mapconcat
          'identity
          `("updatedb -l 0"
            "-U"
            ,directory
            "--add-prunepaths \""
            ,(concat directory ".Trash-1000")
            ,(concat directory ".Trash-1001")
            "\""
            "-o $HOME/.locate.db")
          " "))
        (lambda (process event)
          (minibuffer-message (if (string= "finished\n" event)
                                  "Locate database updated!"
                                "Updating locate database failed!"))))))

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

;; Follow links to version-controlled files
(setq vc-follow-symlinks t)

(defalias 'yes-or-no-p 'y-or-n-p)

;; Leave point at center of the screen when scrolling
(setq scroll-preserve-screen-position t)

;; SHIFT selection
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

;; History navigation
(with-eval-after-load "comint"
  (define-key comint-mode-map [(control ?p)] 'comint-previous-input)
  (define-key comint-mode-map [(control ?n)] 'comint-next-input))

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

;; Delete all whitespace when deleting backward
(setq backward-delete-char-untabify-method 'all)

;; Custom frame title
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

;; Don't let Customize mess with my .emacs
(setq custom-file (concat site-lisp-directory "custom.el"))
(load custom-file 'noerror)

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  "Indent the current buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (untabify-buffer)
  (indent-buffer)
  (delete-trailing-whitespace))
(defalias 'clean-buffer 'cleanup-buffer)

(defun delete-trailing-whitespace-safe ()
  "Delete trailing whitespaces if file is not version controlled
or version controlled but untracked. Make sure to return `nil' in
case it is used in hooks."
  (and (buffer-file-name)
       (or
        (memq (vc-backend (buffer-file-name)) '(nil none))
        ;; git backend and autocommitted repo
        (and (memq (vc-backend (buffer-file-name)) vc-handled-backends)
             (fboundp 'vc-auto-commit-backend)
             (vc-auto-commit-backend)))
       (delete-trailing-whitespace)
       nil))

;; If indent-tabs-mode is off, untabify before saving
(defun untabify-safe ()
  "Untabify buffer if it is not VC or untracked in VC and if
`indent-tabs-mode' is nil."
  (and (buffer-file-name)
       (or
        (memq (vc-backend (buffer-file-name)) '(nil none))
        (and (memq (vc-backend (buffer-file-name)) vc-handled-backends)
             (and (fboundp 'vc-auto-commit-backend)
                  (vc-auto-commit-backend))))
       (not indent-tabs-mode)
       (untabify (point-min) (point-max))
       nil))

(defun cleanup-buffer-safe ()
  (interactive)
  (untabify-safe)
  (delete-trailing-whitespace-safe))

(add-hook 'before-save-hook 'cleanup-buffer-safe)

(defun indent-json (&optional begin end)
  "Run Python's JSON indenter on the buffer"
  (interactive "r")
  (save-excursion
    (shell-command-on-region
     (or begin (point-min)) (or end (point-max)) "python2 -m json.tool"
     (current-buffer)
     t)))

;; Bookmarks
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

(autoload 'zap-up-to-char "misc" "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char)

;; Ignore case when completing
(setq completion-ignore-case t)

;; Filenames too, to browse with dired for example...
(setq read-file-name-completion-ignore-case t)

(defun change-to-utf-8 ()
  (interactive)
  (set-buffer-file-coding-system 'utf-8-unix))

(global-set-key [(f2)] 'change-to-utf-8)

;; Minibuffer history
;;(savehist-mode t)

;; Always add a final newline
(setq require-final-newline t)

;; Display time in modeline
(display-time)
(setq display-time-24hr-format 1)

(setq sentence-end-double-space nil)

(setq x-select-enable-clipboard t        ; copy-paste should work ...
      interprogram-paste-function            ; ...with...
      'x-cut-buffer-or-selection-value)      ; ...other X clients

;; Don't warn when quitting emacs with running processes
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (letf (((symbol-function 'process-list)
          (lambda ()))) ad-do-it))

;; Don't warn when killing running processes
(delq 'process-kill-buffer-query-function
      kill-buffer-query-functions)

;; Make URLs in comments/strings clickable
(add-hook 'find-file-hooks 'goto-address-prog-mode)

;; Enable narrow-to-region binding
(put 'narrow-to-region 'disabled nil)

;; Delete the selected region when something is typed or with DEL
(delete-selection-mode t)

;; Text selection highlighted by default on Emacs 23
;;(transient-mark-mode t)

;; Non interactive function in apropos
;; Make C-h a act as C-u C-h a
(setq apropos-do-all t)

;; Use system trash (for emacs 23)
(setq delete-by-moving-to-trash t)

;; Activate automatic timestamp
(setq
 time-stamp-active t
 time-stamp-format "%04y-%02m-%02d %02H:%02M:%02S (%u)") ; date format

(defun time-stamp-insert ()
  (interactive)
  (insert (if comment-start
              (concat comment-start " ")
            "")
          "Time-stamp: <>"))

(defalias 'insert-time-stamp 'time-stamp-insert)

(add-hook 'before-save-hook 'time-stamp)

(defun insert-euro ()
  "Insert a Euro currency symbol in utf-8."
  (interactive)
  (ucs-insert #x20ac))

;; Make scripts executable on save
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Open bash-fc-* files from fc command or C-x C-e in terminal in sh-mode
(add-to-list 'auto-mode-alist '("bash-fc-[0-9]+\\'" . sh-mode))

(setq truncate-lines nil)
(setq truncate-partial-width-windows nil)

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

(defun kill-emacs-or-frame (arg)
  (interactive "P")
  (if (not server-buffer-clients)
      (if (and (not arg) (> (length (visible-frame-list)) 1))
          (delete-frame)
        (save-buffers-kill-emacs))
    (save-buffer)
    (server-buffer-done (current-buffer))))

(global-set-key "\C-x\C-c" 'kill-emacs-or-frame)

;; Iconify emacs when done editing
(add-hook 'server-done-hook 'iconify-frame)

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


;; Redefine `query-replace-read-from' to add a custom keymap when
;; replacing strings. Now, C-u ENTER does the reverse suggested
;; replacement.
(defvar query-replace-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map [remap exit-minibuffer]
      (lambda ()
        (interactive)
        (if (and current-prefix-arg query-replace-defaults)
            (setq query-replace-defaults
                  (cons
                   (cdr query-replace-defaults)
                   (car query-replace-defaults))))
        (exit-minibuffer)))
    map))

(defun query-replace-read-from (prompt regexp-flag)
  "Query and return the `from' argument of a query-replace operation.
The return value can also be a pair (FROM . TO) indicating that the user
wants to replace FROM with TO."
  (if query-replace-interactive
      (car (if regexp-flag regexp-search-ring search-ring))
    (let* ((history-add-new-input nil)
           (query-replace-defaults query-replace-defaults)
           (prompt
            (if query-replace-defaults
                (format "%s (default %s -> %s): " prompt
                        (query-replace-descr (car query-replace-defaults))
                        (query-replace-descr (cdr query-replace-defaults)))
              (format "%s: " prompt)))
           (from
            ;; The save-excursion here is in case the user marks and copies
            ;; a region in order to specify the minibuffer input.
            ;; That should not clobber the region for the query-replace itself.
            (save-excursion
              (if regexp-flag
                  (read-regexp prompt nil query-replace-from-history-variable)
                (read-from-minibuffer
                 prompt nil query-replace-keymap nil query-replace-from-history-variable
                 (car (if regexp-flag regexp-search-ring search-ring)) t)))))
      (if (and (zerop (length from)) query-replace-defaults)
          (cons (car query-replace-defaults)
                (query-replace-compile-replacement
                 (cdr query-replace-defaults) regexp-flag))
        (add-to-history query-replace-from-history-variable from nil t)
        ;; Warn if user types \n or \t, but don't reject the input.
        (and regexp-flag
             (string-match "\\(\\`\\|[^\\]\\)\\(\\\\\\\\\\)*\\(\\\\[nt]\\)" from)
             (let ((match (match-string 3 from)))
               (cond
                ((string= match "\\n")
                 (message "Note: `\\n' here doesn't match a newline; to do that, type C-q C-j instead"))
                ((string= match "\\t")
                 (message "Note: `\\t' here doesn't match a tab; to do that, just type TAB")))
               (sit-for 2)))
        from))))

;; Trying keyfreq
(require 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

;; Trying multi-term
;; (require 'multi-term)
;; (setq multi-term-program "/bin/zsh")

(require 'multiple-cursors)
(global-set-key (kbd "C-ç") 'mc/mark-next-like-this)
(setq mc/list-file "~/Dropbox/emacs/.mc-lists.el")

(setq set-mark-command-repeat-pop t)

(setq ada-prj-default-gnatmake-opt "-g -gnatW8")

;; (defalias 'read-regexp-old 'read-regexp)
;; (defun read-regexp (prompt &optional defaults history)
;;   "Replace `read-regexp' to run `re-builder'."
;;   (save-window-excursion
;;     (save-excursion
;;       (save-restriction
;;         (re-builder)
;;         (recursive-edit)
;;         (let ((re (with-output-to-string
;;                     (print
;;                      (reb-target-binding
;;                       (prog1
;;                           reb-regexp
;;                         (reb-quit)))))))
;;           (substring re 2 (1- ( 1- (length re)))))))))

;; (defadvice reb-quit (after recursive-quit activate)
;;   (if (> (recursion-depth) 0)
;;       (exit-recursive-edit)))

(defun collect-regexp (regexp &optional beg end)
  "Collect all string matched by REGEXP and store it in the kill
ring."
  (interactive (list (read-regexp "Collect regexp: ")
                     (or (region-beginning) (point))
                     (region-end)))
  (save-excursion
    (goto-char beg)
    (let (acc)
      (while (re-search-forward regexp end t)
        (push (match-string 0) acc))
      (message "Kill-ring: %s" acc)
      (kill-new (mapconcat 'identity (nreverse acc) "\n")))))

;;; init.el ends here
