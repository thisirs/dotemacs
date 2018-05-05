(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-utils)

;; Add personal site-lisp to load-path
(defvar site-lisp-directory "~/CloudStation/Sylvain/emacs/site-lisp/")

;; Add .emacs.d/site-lisp to load path and all sub-directories
(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))

(define-on-macro "knuth")
(define-on-macro "zbook")
(define-on-macro "zouzou")

(modify-all-frames-parameters '((fullscreen . maximized)))

;; Disable dialog box, tool bar...
(setq use-file-dialog nil)
(setq use-dialog-box nil)

(menu-bar-mode -1)

(when window-system
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (horizontal-scroll-bar-mode -1)
  (tooltip-mode -1))

(line-number-mode)
(column-number-mode)
(size-indication-mode)

(setq inhibit-startup-screen t)

(defun display-startup-echo-area-message ()
  (message "Let the hacking begin!"))

(setq visible-bell nil)

;; Use gtklp
(when (executable-find "gtklp")
  (setq lpr-command "gtklp")
  (setq ps-lpr-command "gtklp"))

;; No disabled command like timer-list
(setq disabled-command-function nil)

;; No confirmation because of openwith
(setq large-file-warning-threshold nil)

(setq ring-bell-function 'ignore)

;; Tramp env to properly display dired
(with-eval-after-load "tramp"
  (add-to-list 'tramp-remote-process-environment "LC_ALL=en_US.utf8" 'append))

;; Backups
(setq make-backup-files t ;; do make backups
      ;;  backup-by-copying t     ;; and copy them here
      backup-directory-alist '((".*" . "~/.emacs.d/emacs.backups"))
      version-control t
      kept-new-versions 10
      kept-old-versions 0
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

;; No lockfiles
(setq create-lockfiles nil)

;; Highlight matching paren
(show-paren-mode 1)

(mouse-wheel-mode 1)

(on-zbook
 (electric-pair-mode))

(electric-indent-mode 1)

(let ((bootstrap-file (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el"))
      (bootstrap-version 3))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(straight-use-package 'diminish)
(require 'bind-key)

;; https://github.com/jwiegley/use-package/issues/204#issuecomment-226684009
(defmacro use-package-bq (&rest args)
  "Wrap use-package to use backquote."
  (declare (indent 1))
  (list 'eval (list 'backquote `(use-package ,@args))))

(load "~/CloudStation/Sylvain/emacs/personal.el" :noerror)

;; No confirmation when loading theme
(setq custom-safe-themes t)

;; Loading zenburn theme
;; http://github.com/bbatsov/zenburn-emacs
(use-package zenburn-theme              ; A low contrast color theme for Emacs.
  :straight t
  :if (on-zouzou)
  :if (window-system)
  :config
  (load-theme 'zenburn t))

(use-package solarized                  ; The Solarized color theme, ported to Emacs.
  :if (or (on-zbook) (on-knuth))
  :if (window-system)
  :straight solarized-theme
  :config
  (setq solarized-use-variable-pitch nil)
  (setq solarized-scale-org-headlines nil)
  (load-theme 'solarized-dark t))

;; https://github.com/nashamri/spacemacs-theme
(use-package spacemacs-theme            ; Color theme with a dark and light versions
  :disabled t
  :straight t
  :init
  (load-theme 'spacemacs-dark t))

(require 'init-bindings)
(require 'init-editing)
(require 'init-fill)
(require 'init-find-file)
(require 'init-latex)

(require 'init-auctex)
(require 'init-desktop)
(require 'init-dired)
(require 'init-elisp)
(require 'init-erc)
;; (require 'init-helm)
(require 'init-hippie-expand)
(require 'init-ibuffer)
(require 'init-isearch)
(require 'init-matlab)
(require 'init-midnight)
(require 'init-org)
(require 'init-paredit)
(require 'init-python)
(require 'init-ruby)
(require 'init-scratch)
(require 'init-twittering)
(require 'init-wcheck)
(require 'init-ruby)
(require 'init-python)
(require 'init-yasnippet)
(require 'init-ess)
(require 'init-password)

;; Activate Hunspell
(when (executable-find "hunspell")
  (setq-default ispell-program-name (executable-find "hunspell"))
  (setq ispell-really-hunspell t))
(ignore-errors (ispell-change-dictionary "fr-reforme1990"))
(setq ispell-choices-win-default-height 5)

(use-package abbrev
  :init
  ;; Silently save abbrevs on quitting emacs
  (setq save-abbrevs 'silently))

(use-package academic-phrases
  :straight t)

(use-package ag                         ; A front-end for ag ('the silver searcher'), the C ack replacement.
  :if (executable-find "ag")
  :straight t
  :bind ("M-g f" . ag-search-current-directory)
  :config
  ;; http://github.com/mhayashi1120/Emacs-wgrep/raw/master/wgrep-ag.el
  (use-package wgrep-ag                 ; Writable ag buffer and apply the changes to files
    :straight t
    :bind (:map wgrep-mode-map
                ("C-x s" . wgrep-save-all-buffers)))

  (setq ag-arguments (list "--smart-case" "--stats" "--hidden" "--all-text"))
  (setq ag-group-matches nil)

  (defun ag-search-current-directory (string)
    (interactive (list (ag/read-from-minibuffer "Ag search string")))
    (let ((current-prefix-arg last-prefix-arg))
      (ag/search string default-directory)))) 

;; https://github.com/jwiegley/alert
(use-package alert                      ; Growl-style notification system for Emacs
  :straight t
  :config
  (alert-add-rule :style 'libnotify))

;; https://github.com/syohex/emacs-anzu
(use-package anzu                       ; Display incremental search stats in the modeline.
  :straight t
  :config
  (global-anzu-mode 1)
  :diminish anzu-mode)

;; http://nschum.de/src/emacs/auto-dictionary/
(use-package auto-dictionary            ; automatic dictionary switcher for flyspell
  :straight t
  :bind (("C-c w l" . adict-change-dictionary)
         ("C-c w g" . adict-guess-dictionary))
  :init
  (add-hook 'flyspell-mode-hook #'auto-dictionary-mode))

;; https://github.com/abo-abo/avy
(use-package avy                        ; tree-based completion
  :straight t
  :config
  (setq avy-style 'at)
  (setq avy-keys '(?a ?z ?e ?r ?t ?y ?u ?i ?o ?p
                      ?q ?s ?d ?f ?g ?h ?j ?k ?l ?m
                      ?w ?x ?c ?v ?b ?n))
  (setq avy-background t)
  :bind ("M-h" . avy-goto-subword-1))

;; https://github.com/DamienCassou/beginend
(use-package beginend                   ; Redefine M-< and M-> for some modes
  :straight t
  :config
  (beginend-global-mode))

(use-package cmake-mode :straight t)        ; major-mode for editing CMake sources

(use-package compile
  :defer
  :config
  ;; Move point to first error
  (setq compilation-scroll-output 'first-error))

;; https://github.com/abo-abo/swiper
(use-package counsel                    ; Various completion functions using Ivy
  :straight t
  :bind (("C-x C-f" . counsel-find-file)
         ("C-x l" . counsel-locate)
         ("M-x" . counsel-M-x)
         ("M-y" . counsel-yank-pop)
         ("C-c e l" . counsel-find-library)
         :map help-map
         ("f" . counsel-describe-function)
         ("v" . counsel-describe-variable)
         ("C-l" . counsel-info-lookup-symbol)
         :map ivy-minibuffer-map
         ("M-y" . ivy-next-line))
  :config
  (use-package smex :straight t))

(use-package drag-stuff                 ; Drag stuff (lines, words, region, etc...) around
  :straight t
  :diminish drag-stuff-mode
  :bind (([C-M-S-up] . drag-stuff-up)
         ([C-M-S-down] . drag-stuff-down)
         ("C-M-;" . drag-stuff-left)
         ("C-M-'" . drag-stuff-right))
  :config
  ;; https://github.com/kaushalmodi/.emacs.d/blob/master/setup-files/setup-drag-stuff.el
  ;; http://emacs.stackexchange.com/a/13942/115
  (defvar modi/drag-stuff--point-adjusted nil)
  (defvar modi/drag-stuff--point-mark-exchanged nil)

  (defun modi/drag-stuff--adj-pt-pre-drag ()
    "If a region is selected AND the `point' is in the first column, move
back the point by one char so that it ends up on the previous line. If the
point is above the mark, exchange the point and mark temporarily."
    (when (region-active-p)
      (when (< (point) (mark)) ; selection is done starting from bottom to up
        (exchange-point-and-mark)
        (setq modi/drag-stuff--point-mark-exchanged t))
      (if (zerop (current-column))
          (progn
            (backward-char 1)
            (setq modi/drag-stuff--point-adjusted t))
        ;; If point did not end up being on the first column after the
        ;; point/mark exchange, revert that exchange.
        (when modi/drag-stuff--point-mark-exchanged
          (exchange-point-and-mark) ; restore the original point and mark loc
          (setq modi/drag-stuff--point-mark-exchanged nil)))))

  (defun modi/drag-stuff--rst-pt-post-drag ()
    "Restore the `point' to where it was by forwarding it by one char after
the vertical drag is done."
    (when modi/drag-stuff--point-adjusted
      (forward-char 1)
      (setq modi/drag-stuff--point-adjusted nil))
    (when modi/drag-stuff--point-mark-exchanged
      (exchange-point-and-mark) ; restore the original point and mark loc
      (setq modi/drag-stuff--point-mark-exchanged nil)))

  (add-hook 'drag-stuff-before-drag-hook #'modi/drag-stuff--adj-pt-pre-drag)
  (add-hook 'drag-stuff-after-drag-hook  #'modi/drag-stuff--rst-pt-post-drag))

;; https://github.com/spotify/dockerfile-mode
(use-package dockerfile-mode :straight t)   ; Major mode for editing Docker's Dockerfiles

(use-package dumb-jump                  ; jump to definition for multiple languages without configuration.
  :straight t
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'ivy))

;; ediff settings
(use-package ediff-wind
  :defer t
  :config
  ;; Split windows horizontally in ediff (instead of vertically)
  (setq ediff-split-window-function 'split-window-vertically)

  ;; No separate frame for ediff control buffer
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)

  (setq ediff-diff-options "-w")

  ;; Show all in org files with ediff
  (defun ediff-outline-show-all ()
    (if (eq major-mode 'org-mode)
        (outline-show-all)))

  (add-hook 'ediff-prepare-buffer-hook #'ediff-outline-show-all)

  ;; ediff buffer with file
  (defalias 'ediff-buffer-with-file 'ediff-current-file)

  ;; Restore window configuration after quit
  (add-hook 'ediff-before-setup-hook
            (lambda ()
              (window-configuration-to-register 'ediff)))

  (add-hook 'ediff-quit-hook
            (lambda ()
              (jump-to-register 'ediff))))

(use-package elpy
  :straight t
  :config
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (when (use-package flycheck)
    (add-hook 'elpy-mode-hook 'flycheck-mode))

  (setq python-indent-guess-indent-offset-verbose nil)
  (elpy-enable)
  (setq python-shell-interpreter "ipython3"
        python-shell-interpreter-args "-i --simple-prompt"))

;; https://github.com/hrs/engine-mode
(use-package engine-mode                ; Define and query search engines from within Emacs.
  :straight t
  :bind* ("C-c /" . engine-mode-hydra/body)
  :config

  ;; https://github.com/abo-abo/hydra
  (use-package hydra)                   ; Make bindings that stick around.

  (engine-mode t)

  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :keybinding "d")

  (defengine stack-overflow
    "https://stackoverflow.com/search?q=%s"
    :keybinding "s")

  (defengine google
    "https://google.com/search?q=%s"
    :keybinding "g")

  (defengine wikipedia
    "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
    :keybinding "w")

  (defengine rfcs
    "http://pretty-rfc.herokuapp.com/search?q=%s"
    :keybinding "r")

  (defhydra engine-mode-hydra (:color blue)
    "Engine mode"
    ("d" engine/search-duckduckgo "duckduckgo")
    ("w" engine/search-wikipedia "wikipedia")
    ("r" engine/search-rfcs "RFC")
    ("s" engine/search-stack-overflow "stack overflow")
    ("g" engine/search-google "google")))

(use-package-bq epwdgen                    ; Flexible password generator
  :straight (epwdgen :type git
                     :local-repo ,(expand-file-name "epwdgen" site-lisp-directory))
  :commands epwdgen-generate-password
  :config
  (setq epwdgen-password-presets
        '(("passphrase, 4 words, space separator" passphrase
           :sep " " :file "/home/sylvain/CloudStation/Sylvain/wordlist.lst")
          ("alphanumeric, length 16" password
           :length 16
           :letter mixed
           :number t
           :symbol nil
           :ambiguous t
           :group t)
          ("classic, length 16" password
           :length 16
           :letter mixed
           :number t
           :symbol t
           :ambiguous t
           :group t)
          ("upper+number, length 4" password
           :length 4
           :letter uppercase-only
           :number t
           :symbol nil
           :ambiguous nil
           :group t))))

(use-package eval-expr                  ; enhanced eval-expression command
  :straight t
  :bind ("M-:" . eval-expr)
  :config
  (setq eval-expr-print-function 'pp
        eval-expr-print-level 20
        eval-expr-print-length 100)
  (use-package paredit                  ; minor mode for editing parentheses
    :config
    (defun eval-expr-minibuffer-setup ()
      (add-function :before-until (local 'eldoc-documentation-function)
                    #'elisp-eldoc-documentation-function)
      (eldoc-mode)
      (enable-paredit-mode)
      (add-hook 'completion-at-point-functions
                #'elisp-completion-at-point nil 'local)
      (local-set-key (kbd "<tab>") #'completion-at-point)
      (set-syntax-table emacs-lisp-mode-syntax-table))))

;; Set path as if emacs were run in a terminal
;; https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell       ; Get environment variables such as $PATH from the shell
  :straight t
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "SSH_AGENT_PID")
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))

(use-package expand-region              ; Increase selected region by semantic units.
  :straight t
  :bind (("C-à" . er/expand-region)
         ("C-M-à" . er/contract-region)))

;; Open quickly a temporary file
;; https://github.com/thisirs/find-temp-file.git
(use-package find-temp-file             ; Open quickly a temporary file
  :preface
  (setq find-temp-file-directory "~/CloudStation/Sylvain/drafts")
  (add-hook 'after-init-hook
            (lambda ()
              (interactive)
              (with-current-buffer "*scratch*"
                (setq default-directory (expand-file-name "emacs-lisp-mode" find-temp-file-directory)))))
  :straight t
  :bind ("C-x C-t" . find-temp-file)
  :commands find-temp-file--filename
  :init
  (defun find-temp-file-save-scratch ()
    "Save *scratch* buffer as a draft file."
    (interactive)
    (if (and (get-buffer "*scratch*")
             (with-current-buffer "*scratch*"
               (buffer-modified-p)))
        (with-temp-buffer
          (insert-buffer "*scratch*")
          (let* ((find-temp-template-alist (list (cons "el" "%M/%D/%N-scratch-%T.el")))
                 (file-path (find-temp-file--filename "el")))
            (make-directory (file-name-directory file-path) :parents)
            (write-file file-path)))))
  :config
  (add-to-list 'find-temp-template-alist (cons "elscratch" "%M/%D/%N-scratch-%T.el"))

  ;; Save scratch buffer as temp file when quitting emacs
  (add-hook 'kill-emacs-hook #'find-temp-file-save-scratch)

  (setq find-temp-template-default "%M/%D/%N-%T.%E")
  (add-to-list 'find-temp-template-alist (cons "m" "%M/%D/%N_%T.%E")))

;; Diminish
(eval-after-load "undo-tree"
  '(diminish 'undo-tree-mode))
(eval-after-load 'elisp-slime-nav
  '(diminish 'elisp-slime-nav-mode))

;; http://www.flycheck.org
(use-package flycheck                   ; On-the-fly syntax checking
  :straight t
  :commands global-flycheck-mode
  :defer 10
  :config
  (global-flycheck-mode 1)
  (setq-default flycheck-disabled-checkers
                '(emacs-lisp emacs-lisp-checkdoc tex-chktex tex-lacheck))
  (setq flycheck-lintr-linters "with_defaults(commented_code_linter = NULL, line_length_linter = line_length_linter(120))")

  (use-package flycheck-color-mode-line ; Change mode line color with Flycheck status
    :straight t
    :config
    (setq flycheck-highlighting-mode 'symbols)
    (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)))

;; https://github.com/magit/git-modes
(use-package gitconfig-mode :straight t)    ; Major mode for editing .gitconfig files

;; https://github.com/magit/git-modes
(use-package gitignore-mode :straight t)    ; Major mode for editing .gitignore files

(use-package google-translate-smooth-ui ; Emacs interface to Google Translate.
  :straight google-translate
  :config
  (require 'google-translate-smooth-ui)
  (setq google-translate-translation-directions-alist
        '(("en" . "fr") ("fr" . "en")))
  (setq google-translate-listen-program
        (or (executable-find "mplayer")
            (executable-find "vlc")))
  :bind ("C-c t" . google-translate-smooth-translate))

(use-package grep
  :defer
  :bind (:map grep-mode-map
              ("a" . grep-toggle-binary-search))
  :config
  (if (not (executable-find "ag"))
      (bind-key "M-g f" 'find-grep))

  ;; grep binary files (latin-1 are binary files...)
  (defun grep-toggle-binary-search (&optional arg)
    "Toggle \"grep\"/\"grep -a\" in grep command."
    (interactive "P")
    (if (stringp (car compilation-arguments))
        (setcar compilation-arguments
                (replace-regexp-in-string
                 "grep\\( -a\\)?"
                 (lambda (m) (if (match-string 1 m) "grep" "grep -a"))
                 (car compilation-arguments))))
    (grep-compute-defaults)
    (let* (msg
           (grep-find-cmd (replace-regexp-in-string
                           "grep\\( -a\\)?"
                           (lambda (m)
                             (if (match-string 1 m)
                                 (progn (setq msg "Binary search in grep is off") "grep")
                               (setq msg "Binary search in grep is on")
                               "grep -a"))
                           (car grep-find-command)))
           (point (progn (string-match "\\\\{\\\\}" grep-find-cmd) (match-beginning 0))))
      (grep-apply-setting 'grep-find-command
                          (cons grep-find-cmd point))
      (message msg))))

;; https://github.com/kai2nenobu/guide-key
(use-package guide-key                  ; Guide the following key bindings automatically and dynamically
  :straight t
  :disabled t
  :diminish guide-key-mode
  :commands guide-key-mode
  :defer 10
  :config
  (setq guide-key/guide-key-sequence
        '("C-c p"
          "C-x r"
          "C-x 4"
          "C-x 5"
          (org-mode "C-c C-x")
          (smerge-mode "C-c ^")))
  (guide-key-mode 1))

(use-package hl-line
  :disabled
  :straight t
  :config
  ;; Highlight the line only in the active window
  (setq global-hl-line-sticky-flag t)
  (setq hl-line-sticky-flag t)

  ;; hl-line+
  ;; http://www.emacswiki.org/emacs/hl-line+.el
  (use-package hl-line+               ; Extensions to hl-line.el.
    :straight t
    :config
    (toggle-hl-line-when-idle 1) ; Highlight line only when idle
    ;; Number of seconds of idle time after when the line should be highlighted
    (setq hl-line-idle-interval 5)
    ;; Number of seconds for `hl-line-flash' to highlight the line
    (setq hl-line-flash-show-period 3)))

;; https://www.emacswiki.org/emacs/download/info%2b.el
(use-package info+ :straight t)             ; Extensions to `info.el'.

;; http://github.com/nonsequitur/inf-ruby
(use-package inf-ruby :straight t)          ; Run a Ruby process in a buffer

(use-package ivy-bibtex                 ; A bibliography manager based on Ivy
  :straight (ivy-bibtex :type git :host github :repo "thisirs/helm-bibtex"
                        :files ("ivy-bibtex.el" "bibtex-completion.el"))

  :defer 5
  :bind ("C-x b" . ivy-bibtex)
  :config
  (setq bibtex-completion-bibliography
        '("~/CloudStation/Sylvain/recherche/biblio/zotero/refs.bib"))
  (setq bibtex-completion-library-path
        '("~/CloudStation/Sylvain/recherche/biblio/zotero/tracking/"
          "~/CloudStation/Sylvain/recherche/biblio/zotero/compressed_sensing/"
          "~/CloudStation/Sylvain/recherche/biblio/zotero/hashing/"
          "~/CloudStation/Sylvain/recherche/biblio/zotero/graphs_and_deep_learning/"
          "~/CloudStation/Sylvain/recherche/biblio/zotero/NN regularization/"
          "~/CloudStation/Sylvain/recherche/biblio/zotero/books/"))
  (setq bibtex-completion-cite-prompt-for-optional-arguments nil)
  (setq bibtex-completion-pdf-field "file")

  ;; Always cite with \cite
  (defun bibtex-completion-format-always-cite (oldfun keys)
    (cl-flet ((completing-read (&rest _) "cite"))
      (funcall oldfun keys)))

  (advice-add 'bibtex-completion-format-citation-cite :around
              #'bibtex-completion-format-always-cite))

;; https://github.com/abo-abo/swiper
(use-package ivy                        ; Incremental Vertical completYon
  :straight t
  :diminish (ivy-mode . "")
  :bind (("C-x C-b" . ivy-switch-buffer)
         ("C-x j" . ivy-bookmarks))
  :config
  (define-key ivy-switch-buffer-map (kbd "C-b") 'next-line)

  ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-use-selectable-prompt t)

  ;; number of result lines to display
  (setq ivy-height 10)

  (ivy-mode)

  (defun ivy-bookmarks-display-transformer (candidate)
    (let* ((width (1- (frame-width)))
           (p1 60.0)
           (p2 30.0)
           (w1 (floor (/ (* (- width 2) p1) 100)))
           (w2 (floor (/ (* (- width 2) p2) 100)))
           (w3 (- width (+ 2 w1 w2)))
           (idx (get-text-property 0 'idx candidate))
           (entry (cdr (nth idx (ivy-state-collection ivy-last)))))

      (concat (truncate-string-to-width candidate w1 0 ?\ )
              " "
              (truncate-string-to-width (concat (cdr (assoc "href" entry))) w2 0 ?\ )
              " "
              (truncate-string-to-width (concat (cdr (assoc "tags" entry))) w3 0 ?\ ))))

  (defun ivy-bookmarks-open (candidate)
    (let ((key (cdr (assoc "href" (cdr candidate)))))
      (browse-url key)))

  (ivy-set-display-transformer
   'ivy-bookmarks
   'ivy-bookmarks-display-transformer)

  (defun ivy-bookmarks ()
    (interactive)
    (let* ((fn "~/.mozilla/firefox/bookmark_list.el")
           candidates)
      (when (file-exists-p fn)
        (setq candidates (with-temp-buffer
                           (insert-file-contents fn)
                           (goto-char (point-min))
                           (read (current-buffer)))))
      (ivy-read (concat "Visit bookmark" (if (null candidates) " (not found)") ": ")
                candidates
                :action 'ivy-bookmarks-open
                :caller 'ivy-bookmarks))))

;; https://github.com/joshwnj/json-mode
(use-package json-mode                  ; json beautifier and more
  :straight t
  :commands json-mode)

(use-package keyfreq                    ; track command frequencies
  :straight t
  :config
  (let ((filepath (format "~/CloudStation/Sylvain/emacs/.emacs.%s.keyfreq" (system-name))))
    (make-directory (file-name-directory filepath) :parents)
    (setq keyfreq-file (format "~/CloudStation/Sylvain/emacs/.emacs.%s.keyfreq" (system-name))))
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;; https://github.com/mhayashi1120/Emacs-langtool
(use-package langtool                   ; Grammar check utility using LanguageTool
  :straight t
  :defer
  :config
  (setq langtool-java-bin "/usr/lib/jvm/java-8-openjdk/jre/bin/java")
  (setq langtool-java-classpath "/usr/share/languagetool:/usr/share/java/languagetool/*")
  (setq langtool-default-language "fr"))

;; http://immerrr.github.com/lua-mode
(use-package lua-mode :straight t)          ; a major-mode for editing Lua scripts

;; https://github.com/joddie/macrostep
(use-package macrostep                  ; interactive macro expander
  :straight t
  :bind ("C-c e m" . macrostep-expand))

;; https://github.com/magit/magit
(use-package magit                      ; A Git porcelain inside Emacs
  :straight t
  :bind ("C-c i" . magit-status)
  :init
  ;; Taken from http://endlessparentheses.com/easily-create-github-prs-from-magit.html
  (defun visit-pull-request-url ()
    "Visit the current branch's PR on Github."
    (interactive)
    (browse-url
     (format "https://github.com/%s/pull/new/%s"
             (replace-regexp-in-string
              "\\`.+github\\.com/\\(.+\\)\\.git\\'" "\\1"
              (magit-get "remote"
                         (or (magit-get-remote)
                             (magit-get-push-remote))
                         "url"))
             (magit-get-current-branch))))
  :config
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)

  ;; Let auth-source handle the passwords for me
  (setq magit-process-find-password-functions '(magit-process-password-auth-source))

  (setq magit-push-always-verify nil)
  (define-key magit-mode-map (kbd "C-o")
    (lambda ()
      (interactive)
      (let ((current-prefix-arg t))
        (call-interactively 'magit-diff-visit-file))))
  (define-key magit-mode-map "V" #'visit-pull-request-url))

;; https://github.com/vermiculus/magithub
(use-package magithub                   ; Magit interfaces for GitHub
  :disabled t
  :after magit
  :config (magithub-feature-autoinject t))

;; http://jblevins.org/projects/markdown-mode/
(use-package markdown-mode              ; Major mode for Markdown-formatted text
  :straight t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.Rmd\\'" . rmarkdown-mode))
  :bind (:map markdown-mode-map
              ("C-c C-f" . nil)
              ("C-c C-f b" . markdown-insert-bold)
              ("C-c C-f i" . markdown-insert-italic)
              ("C-c C-f c" . markdown-insert-code)
              ("C-c C-f l" . markdown-insert-link)
              ("C-c C-f u" . markdown-insert-uri))
  :config
  ;; https://github.com/Fanael/edit-indirect
  (use-package edit-indirect :straight t)   ; Edit regions in separate buffers

  (setq markdown-enable-math t)
  (setq markdown-command
        "pandoc -f markdown+smart -t html5 -s --self-contained 2> /dev/null")

  (define-derived-mode rmarkdown-mode markdown-mode "Rmarkdown"
    "Mode for RMarkdown"
    (set (make-local-variable 'markdown-command-needs-filename) t)
    (set (make-local-variable 'markdown-command) (expand-file-name "rmarkdown-render" user-emacs-directory))))

(use-package mu4e
  :load-path "/usr/share/emacs/site-lisp/mu4e"
  :config
  (use-package state
    :straight t
    :config
    (state-define-state mu4e
      :key "u"
      :in (memq major-mode '(mu4e-main-mode mu4e-headers-mode mu4e-view-mode mu4e-compose-mode))
      :exist (mu4e-running-p)
      :create (progn
                (mu4e)
                (delete-other-windows))))

  (setq mu4e-compose-keep-self-cc nil)
  (setq mu4e-compose-dont-reply-to-self t)

  (setq mu4e-context-policy 'pick-first)

  (setq mu4e-compose-context-policy nil)

  (setq mu4e-compose-format-flowed t)

  (setq mu4e-use-fancy-chars t)

  (setq mu4e-headers-leave-behavior 'apply)

  ;; Don't save message to Sent Messages, Gmail/IMAP takes care of this
  (setq mu4e-sent-messages-behavior 'delete)

  ;; Use mu4e for e-mail in emacs
  (setq mail-user-agent 'mu4e-user-agent)

  (setq mu4e-change-filenames-when-moving t)

  ;; Exit after sending message
  (setq message-kill-buffer-on-exit t)

  ;; No AM date
  (setq mu4e-headers-time-format "%T")
  (setq mu4e-headers-date-format "%Y-%m-%d %H:%M")

  (setq mu4e-headers-fields '((:human-date . 16)
                              (:flags . 6)
                              (:mailing-list . 10)
                              (:from . 22)
                              (:subject)))

  (setq mu4e-get-mail-command "mbsync -a")

  (if (on-zbook)
      (setq mu4e-update-interval 500)
    (setq mu4e-update-interval nil))

  (setq mu4e-maildir "~/mbsync")

  (define-key-after global-map [menu-bar tools mu4e]
    (cons "Mu4e" (make-sparse-keymap " blah")) 'tools)

  (define-key global-map [menu-bar tools mu4e dau]
    (cons "Disable auto-update"
          (lambda ()
            (interactive)
            (cancel-timer mu4e~update-timer)
            (setq mu4e-update-interval nil))))

  (require 'org-mu4e)

  (use-package mu4e-alert
    :if (on-zbook)
    :straight t
    :config
    (mu4e-alert-set-default-style 'libnotify)
    (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
    (setq mu4e-alert-interesting-mail-query
          (concat
           "flag:unread"
           " AND NOT flag:trashed"
           " AND NOT maildir:/gmail/INBOX"))))

;; Using multi-term instead of term
;; http://www.emacswiki.org/emacs/download/multi-term.el
(use-package multi-term                 ; Managing multiple terminal buffers in Emacs.
  :straight t
  :config
  (defalias 'term 'multi-term)
  (setq multi-term-program "/bin/zsh"))

(use-package multiple-cursors           ; Multiple cursors for Emacs.
  :straight t
  :init
  (setq mc/list-file "~/CloudStation/Sylvain/emacs/.mc-lists.el")
  :config
  (add-to-list 'mc/unsupported-minor-modes 'electric-pair-mode)
  :bind (("C-M-c" . mc/mark-next-like-this)))

(use-package octave
  :defer t
  :config
  (define-key octave-mode-map "\C-c\C-r" #'octave-send-region)
  (define-key octave-mode-map "\C-c\C-s" #'octave-send-buffer))

;; https://bitbucket.org/jpkotta/openwith
(use-package openwith                   ; Open files with external programs
  :straight t
  :preface
  (rassq-delete-all #'doc-view-mode-maybe auto-mode-alist)
  :config
  (setq openwith-associations
        '(("\\.pdf\\'" "evince" (file))
          ("\\.\\(?:mpe?g\\|avi\\|wmv\\|flv\\)\\'" "vlc" (file))
          ("\\.\\(od[ts]\\|docx?\\|xlsx?\\)\\'" "soffice" (file))))
  (openwith-mode))

;; Contextual capture and agenda commands for Org-mode
;; https://github.com/thisirs/org-context
(use-package org-context                ; Contextual capture and agenda commands for Org-mode
  :straight t
  :config (org-context-activate))

;; https://github.com/Malabarba/paradox
(use-package paradox                    ; A modern Packages Menu. Colored, with package ratings, and customizable.
  :straight t
  :commands paradox-list-packages
  :config
  (setq paradox-github-token t)
  (setq paradox-execute-asynchronously t))

(use-package pcache :straight t)            ; persistent caching for Emacs.

(use-package pdf-tools                  ; Support library for PDF documents.
  :straight t
  :defer 10
  :init
  ;; pdf-annot-minor-mode before pdf-sync-minor-mode
  (setq pdf-tools-enabled-modes
        '(pdf-history-minor-mode
          pdf-isearch-minor-mode
          pdf-links-minor-mode
          pdf-misc-minor-mode
          pdf-outline-minor-mode
          pdf-misc-size-indication-minor-mode
          pdf-misc-menu-bar-minor-mode
          pdf-sync-minor-mode
          pdf-annot-minor-mode
          pdf-misc-context-menu-minor-mode
          pdf-cache-prefetch-minor-mode
          pdf-occur-global-minor-mode))

  :config
  (pdf-tools-install :force-compile nil :no-error)
  (define-key pdf-view-mode-map (kbd "M-w") 'pdf-view-kill-ring-save)

  (setq pdf-misc-print-programm lpr-command)

  (use-package pdf-annot
    :config
    (defun pdf-annot-add-text-annotation-here (ev)
      (interactive "@e")
      (pdf-annot-activate-annotation (pdf-annot-mouse-add-text-annotation ev)))

    (defun pdf-annot-edit-contents-abort-or-delete ()
      "Abort current annotation or delete if empty"
      (interactive)
      (if (zerop (buffer-size))
          (pdf-annot-delete-current)
        (pdf-annot-edit-contents-abort)))

    (defun pdf-annot-delete-current ()
      "Delete currently edited annotation"
      (interactive)
      (pdf-annot-delete pdf-annot-edit-contents--annotation)
      (pdf-annot-edit-contents-abort))

    :bind (:map
           pdf-annot-minor-mode-map
           ([double-mouse-1] . pdf-annot-add-text-annotation-here)
           :map
           pdf-annot-edit-contents-minor-mode-map
           ;; Instead of C-c C-q
           ("C-c C-k" . pdf-annot-edit-contents-abort-or-delete)
           ("C-c C-d" . pdf-annot-delete-current)))

  ;; https://github.com/thisirs/pdf-tools-points.git
  (use-package-bq pdf-tools-points          ; Offline annotation with pdf-tools and tikz
    :straight (pdf-tools-points :local-repo ,(expand-file-name "pdf-tools-points" site-lisp-directory))))

;; https://github.com/ejmr/php-mode
(use-package php-mode :straight t)          ; Major mode for editing PHP code

;; https://github.com/vitoshka/polymode
(use-package polymode                   ; Versatile multiple modes with extensive literate programming support
  :commands poly-markdown-string-mode
  :config
  (defcustom  pm-inner/markdown-string
    (pm-hbtchunkmode "markdown"
                     :mode 'markdown-mode
                     :head-reg "{%markdown%}\n"
                     :tail-reg "{%/markdown%}")
    "Markdown typical chunk."
    :group 'innermodes
    :type 'object)

  (defcustom pm-poly/markdown-string
    (pm-polymode-one "markdown"
                     :hostmode 'pm-host/R
                     :innermode 'pm-inner/markdown-string)
    "Markdown typical polymode."
    :group 'polymodes
    :type 'object)

  (define-polymode poly-markdown-string-mode pm-poly/markdown-string))

;; Projectile
;; https://github.com/bbatsov/projectile
(use-package projectile                 ; Manage and navigate projects in Emacs easily
  :straight t
  :init
  (run-with-idle-timer 10 nil #'projectile-cleanup-known-projects)

  (setq projectile-known-projects-file
        (expand-file-name "cache/projectile-bookmarks.eld" user-emacs-directory))
  (defun projectile-custom-mode-line ()
    (if (projectile-project-p)
        (let* ((project-name (projectile-project-name))
               (project-name-mode-line (if (> (length project-name) 12)
                                           (substring project-name 0 8)
                                         project-name)))
          (format " Pj[%s]" project-name-mode-line))
      ""))
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  ;; Open root directory when switching
  (setq projectile-switch-project-action 'projectile-find-file)

  (setq-default projectile-mode-line '(:eval (projectile-custom-mode-line)))
  (setq projectile-completion-system 'ivy)

  (setq projectile-cache-file
        (expand-file-name "cache/projectile.cache" user-emacs-directory))

  (defun UTC-semester-from-time (time)
    "Return the semester corresponding to TIME."
    (let* ((dtime (decode-time time))
           (month (nth 4 dtime))
           (year (nth 5 dtime)))
      (if (and (< month 8) (> month 2))
          (list (format "P%d" year) (format "%d(1)" year))
        (if (<= month 2)
            (list (format "A%d" (1- year)) (format "%d(2)" (1- year)))
          (list (format "A%d" year) (format "%d(2)" year))))))

  (defun projectile-root-hardcoded (dir &optional list)
    (--some (if (string-prefix-p (abbreviate-file-name it)
                                 (abbreviate-file-name dir)) it)
            (append (-filter #'file-exists-p
                             (mapcar (lambda (args)
                                       (apply #'format "~/CloudStation/Sylvain/enseignements/%s/%s/%s" args))
                                     (let ((semesters (list (car (UTC-semester-from-time (current-time)))
                                                            (car (UTC-semester-from-time
                                                                  (time-add
                                                                   (current-time)
                                                                   (seconds-to-time (* 60 60 24 365 .5)))))))
                                           (uvs '("SY02" "TIS02" "SY09" "SY19"))
                                           (dirs '("Cours" "TP" "TD" "poly" "")))
                                       (-table-flat 'list semesters uvs dirs))))
                    '("~/Dropbox/Documents-sy09/"
                      "~/CloudStation/Sylvain/emacs/site-lisp/"))))

  (add-to-list 'projectile-project-root-files-functions 'projectile-root-hardcoded)

  (defun projectile-ignored-semester (truename)
    "Ignore past semesters."
    (and (string-match "\\([AP]\\)\\([0-9]\\{4\\}\\)" truename)
         (let ((semester (concat (match-string 2 truename)
                                 (if (string= (match-string 1 truename) "P")
                                     "(1)" "(2)")))
               (current-semester (cadr (UTC-semester-from-time (current-time)))))
           (and (string-lessp semester current-semester)
                (not (string= semester current-semester))))))

  (setq projectile-require-project-root nil)

  (setq projectile-ignored-project-function #'projectile-ignored-semester)

  (projectile-global-mode))

;; http://elpa.gnu.org/packages/rainbow-mode.html
(use-package rainbow-mode :straight t)      ; Colorize color names in buffers

;; From https://github.com/jwiegley/dot-emacs
(use-package recentf
  :defer 10
  :commands (recentf-mode
             recentf-add-file
             recentf-apply-filename-handlers)
  :preface
  (defun recentf-add-dired-directory ()
    (if (and dired-directory
             (file-directory-p dired-directory)
             (not (string= "/" dired-directory)))
        (let ((last-idx (1- (length dired-directory))))
          (recentf-add-file
           (if (= ?/ (aref dired-directory last-idx))
               (substring dired-directory 0 last-idx)
             dired-directory)))))
  :init
  (add-hook 'dired-mode-hook 'recentf-add-dired-directory)
  :config
  (recentf-mode 1))

;; Smart modeline
;; http://github.com/Malabarba/smart-mode-line
(use-package smart-mode-line            ; A color coded smart mode-line.
  :if (window-system)
  :straight t
  :commands sml/setup
  :demand t
  :init
  (setq sml/theme 'respectful
        sml/shorten-directory t
        sml/shorten-modes t
        sml/name-width 40
        sml/mode-width 'full)
  :config
  (sml/setup))

(use-package smartparens                ; Automatic insertion, wrapping and paredit-like navigation with user defined pairs.
  :straight t
  :disabled t
  :config
  (sp-local-pair
   '(markdown-mode gfm-mode)
   "\`\`\`" "\`\`\`" :post-handlers '(("||\n" "RET"))))

(use-package saveplace
  :config
  (setq save-place-file "~/.emacs.d/cache/.saveplace")
  (save-place-mode))

(use-package server
  :if (window-system)
  :config
  (unless (server-running-p server-name)
    (server-start)))

(use-package skeletor                   ; Provides project skeletons for Emacs
  :straight t
  :defer 10)

;; https://github.com/yuya373/emacs-slack
(use-package slack                      ; Slack client for Emacs
  :commands (slack-start)
  :init
  (setq slack-buffer-emojify t)
  (setq slack-prefer-current-team t)
  :config
  (slack-register-team
   :name (plist-get (car (auth-source-search :require '(:client_id))) :name)
   :default t
   :client-id (plist-get (car (auth-source-search :require '(:name :client_id))) :client_id)
   :client-secret (plist-get (car (auth-source-search :require '(:name :client_id))) :client_secret)
   :token (plist-get (car (auth-source-search :require '(:name :client_id))) :token)
   ;; :subscribed-channels '(test-rename rrrrr)
   :full-and-display-names t))

(use-package smart-mark                 ; Restore point after C-g when mark
  :straight t
  :config
  (smart-mark-mode 1))

;; Minor mode to resolve diff3 conflicts
(use-package smerge-mode
  :defer 10
  :commands smerge-mode
  :config
  (defun sm-try-smerge ()
    "Turn on smerge-mode if there is a diff marker."
    (let ((old-point (point)))
      (goto-char (point-min))
      (if (re-search-forward "^\\(<\\)\\{7\\} " nil t)
          (smerge-mode 1)
        (goto-char old-point))))
  (add-hook 'find-file-hook 'sm-try-smerge t))

;; https://github.com/thisirs/state.git
(use-package state                      ; Quick navigation between workspaces
  :straight t
  ;; Override state's keymap binding
  :bind-keymap ("s-s" . state-prefix-map)
  :config
  (state-define-state agenda
    :key "a"
    :in (eq major-mode 'org-agenda-mode)
    :switch org-agenda-list)

  (state-define-state debug
    :key "d"
    :switch "*debug*")

  (state-define-state gnus
    :key "g"
    :in (memq major-mode '(gnus-article-edit-mode
                           gnus-article-mode
                           gnus-bookmark-bmenu-mode
                           gnus-browse-mode
                           gnus-category-mode
                           gnus-custom-mode
                           gnus-edit-form-mode
                           gnus-group-mode
                           gnus-kill-file-mode
                           gnus-score-mode
                           gnus-server-mode
                           gnus-sticky-article-mode
                           gnus-summary-mode
                           gnus-tree-mode
                           message-mode
                           plstore-mode
                           sieve-manage-mode
                           sieve-mode
                           smime-mode))
    :exist gnus-alive-p
    :create gnus)

  (state-define-state slack
    :key "l"
    :in (memq major-mode '(slack-message-buffer-mode))
    :switch (tracking-next-buffer)
    :keep (tracking-next-buffer))

  (state-define-state erc
    :key "i"
    :in (and (fboundp 'erc-buffer-list)
             (memq (current-buffer) (erc-buffer-list)))
    :switch (progn (erc-start-or-switch 1)
                   (delete-other-windows))
    :keep (erc-track-switch-buffer 0))

  (state-define-state message
    :key "m"
    :switch "*Messages*")

  (state-define-state scratch
    :key "s"
    :switch "*scratch*")

  (state-define-state twit
    :key "t"
    :in (and (require 'twittering-mode nil t) (twittering-buffer-p))
    :switch twit)

  (state-define-state programming_samples
    :key "r"
    :switch "~/CloudStation/Sylvain/Org/programming_samples.org")

  (state-define-state personnal
    :key "p"
    :switch "~/CloudStation/Sylvain/Org/personnel.org.gpg"
    :keep (org-password-manager-get-pass))

  ;; Switch to init.el or any already open lisp/init-*.el files
  (state-define-state emacs
    :key "e"
    :in (or (state--in-in-file "~/.emacs.d/init.el")
            (state--in-in-file "~/.emacs.d/lisp/init"))
    :exist (or (state--find-file-name-prefix-buffer "~/.emacs.d/init.el")
               (state--find-file-name-prefix-buffer "~/.emacs.d/lisp/init"))
    :switch (let ((buffer (or (state--find-file-name-prefix-buffer "~/.emacs.d/init.el")
                              (state--find-file-name-prefix-buffer "~/.emacs.d/lisp/init")
                              (error "Unable to switch to `%s' state" name))))
              (state--switch-switch-buffer buffer))
    :create (find-file "~/.emacs.d/init.el")
    :keep (unless (eq (current-buffer) (find-buffer-visiting "~/.emacs.d/init.el"))
            (find-file "~/.emacs.d/init.el")))

  ;; Bound state: only accessible from emacs state
  (state-define-state emacs-term
    :key "z"
    :bound emacs
    :exist (get-buffer "*ansi-term (dotemacs)*")
    :in (equal (buffer-name) "*ansi-term (dotemacs)*")
    :switch (if (get-buffer-window "*ansi-term (dotemacs)*")
                (select-window (get-buffer-window "*ansi-term (dotemacs)*"))
              (switch-to-buffer-other-window "*ansi-term (dotemacs)*"))
    :create (progn
              (switch-to-buffer-other-window (current-buffer))
              (ansi-term "/bin/zsh" "ansi-term (dotemacs)")))

  ;; Not bound state with same key
  (state-define-state term
    :key "z"
    :exist (get-buffer "*ansi-term*")
    :in (equal (buffer-name) "*ansi-term*")
    :switch (if (get-buffer-window "*ansi-term*")
                (select-window (get-buffer-window "*ansi-term*"))
              (switch-to-buffer-other-window "*ansi-term*"))
    :create (progn
              (switch-to-buffer-other-window (current-buffer))
              (ansi-term "/bin/zsh")))

  ;; Common pattern when defining a repl state
  (defmacro state-define-repl (name key buffer-name from create)
    `(state-define-state
       ,name
       :bound ,from
       :key ,key
       :exist (get-buffer ,buffer-name)
       :in (equal (buffer-name) ,buffer-name)
       :switch (if (get-buffer-window ,buffer-name)
                   (select-window (get-buffer-window ,buffer-name))
                 (switch-to-buffer-other-window ,buffer-name))
       :create (progn
                 (switch-to-buffer-other-window (current-buffer))
                 ,create)))

  (state-define-repl elisp-repl "j" "*ielm*" (memq major-mode '(lisp-interaction-mode emacs-lisp-mode)) (ielm))
  (state-define-repl matlab-repl "j" "*MATLAB*" (eq major-mode 'matlab-mode) (matlab-shell))
  (state-define-repl python-repl "j" "*Python*" (eq major-mode 'python-mode) (call-interactively 'run-python))
  (state-define-repl ruby-repl "j" "*ruby*" (eq major-mode 'ruby-mode) (inf-ruby))
  (state-define-repl R-repl "j" "*R*" (eq major-mode 'ess-mode) (let (ess-ask-for-ess-directory) (R)))

  (state-global-mode 1))

;; http://www.emacswiki.org/elisp/tidy.el
(use-package tidy :straight t)              ; Interface to the HTML Tidy program

(use-package transpose-frame            ; Transpose windows arrangement in a frame
  :straight t
  :bind ("<C-kp-multiply>" . rotate-frame-anticlockwise))

;; http://www.dr-qubit.org/emacs.php
(use-package undo-tree                  ; Treat undo history as a tree
  :straight t
  :config
  (global-undo-tree-mode)
  (define-key undo-tree-visualizer-mode-map (kbd "RET")
    'undo-tree-visualizer-quit))

;; https://github.com/purcell/unfill
(use-package unfill                     ; Unfill paragraphs or regions, and toggle between filled & unfilled
  :straight t
  :bind ("M-q" . unfill-toggle))

;; https://github.com/thisirs/vc-check-status
(use-package vc-check-status            ; Warn you when quitting emacs and leaving repo dirty.
  :straight t
  :defer 5
  :config
  ;; Be sure to leave my packages' repo on master
  (push '("~/CloudStation/Sylvain/emacs/site-lisp/" (not-on-branch "master")) vc-check-alist)

  ;; Only look for unpushed commits on master
  (push '("~/.emacs.d" (unpushed "master") changes) vc-check-alist)

  ;; Don't check on auto-committed repo
  (add-to-list 'vc-check-cancel-hook
               (lambda ()
                 (and
                  (fboundp 'vc-auto-commit-backend)
                  (vc-auto-commit-backend))))

  (vc-check-status-activate))

;; https://github.com/benma/visual-regexp.el/
(use-package visual-regexp              ; A regexp/replace command for Emacs with interactive visual feedback
  :straight t
  :commands (vr/query-replace vr/replace)
  :bind* (("C-c r" . vr/replace)
          ("C-c q" . vr/query-replace))
  :config
  ;; https://github.com/benma/visual-regexp-steroids.el/
  (use-package visual-regexp-steroids   ; Extends visual-regexp to support other regexp engines
    :straight t
    :commands (vr/select-replace vr/select-query-replace)))

;; http://github.com/thisirs/vc-auto-commit.git
(use-package vc-auto-commit             ; Auto-committing feature for your repository
  :straight t
  :defer 5
  :commands (vc-auto-commit-backend)
  :bind ("C-x v C" . vc-auto-commit)
  :config
  (defun not-on-zbook (root backend)
    (not (on-zbook)))
  (add-hook 'vc-auto-commit-cancel-hook #'not-on-zbook)
  (vc-auto-commit-activate))

(use-package webjump
  :bind ("C-c j" . webjump))

;; http://github.com/mhayashi1120/Emacs-wgrep/raw/master/wgrep.el
(use-package wgrep                      ; Writable grep buffer and apply the changes to files
  :straight t)

(use-package whitespace
  :config
  (setq whitespace-style
        '(face trailing tabs))
  (global-whitespace-mode))

(use-package winner
  :if (not noninteractive)
  :defer 5
  :bind (("M-N" . winner-redo)
         ("M-P" . winner-undo))
  :config
  (setq winner-boring-buffers
        '("*Completions*"
          "*helm for files*"
          "*helm find-file*"
          "*helm complete*"
          "*helm M-x*"
          "*Ibuffer*"
          "*Calendar*"
          "*helm*"))
  (winner-mode 1))

;; wtf for acronym lookup
(use-package wtf :commands wtf-is)

;; Buffers can't have the same name
(with-eval-after-load 'uniquify
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  (setq uniquify-after-kill-buffer-p t))

(use-package yaml-mode :straight t)         ; Major mode for editing YAML files

(use-package yasnippet-snippets
  :straight (yasnippet-snippets
             :type git
             :host github
             :repo "AndreaCrotti/yasnippet-snippets"
             :files ("*.el" ("snippets" "snippets/python-mode"))))

;; http://www.emacswiki.org/zoom-frm.el
(use-package zoom-frm                   ; Commands to zoom frame font size.
  :straight t
  :bind (("C-<down-mouse-4>" . zoom-in)
         ("C-<down-mouse-5>" . zoom-out)
         ("C-c +" . zoom-in)
         ("C-c -" . zoom-out)
         ("<C-kp-add>" . zoom-in)
         ("<C-kp-subtract>" . zoom-out)))

;; https://github.com/thierryvolpiatto/zop-to-char
(use-package zop-to-char                ; A replacement of zap-to-char.
  :straight t
  :bind (("M-z" . zop-to-char)
         ("M-Z" . zop-up-to-char)))

(defun switch-to-external-terminal (&optional arg)
  "Switch to an external terminal. Change directory if ARG is non-nil."
  (interactive "P")
  (if (display-graphic-p)
      (switch-to-tmux arg)
    (suspend-emacs (if arg (format "cd \"%s\"" (file-truename default-directory))))))

(defun tmux-display (message)
  (with-temp-buffer
    (shell-command (format "tmux display -p \"%s\"" message) (current-buffer))
    (string-trim (buffer-string))))

(defun switch-to-tmux-or-suspend (&optional arg)
  "Switch to tmux if in a graphic session. Otherwise, suspend emacs.
Change directory to `default-directory' if ARG is non-nil."
  (interactive "P")
  (if (display-graphic-p)
      (switch-to-tmux arg)
    (suspend-emacs (if arg (format "cd \"%s\"" (file-truename default-directory))))))

(defun tmux-has-session ()
  (eq 0 (shell-command "tmux -q has-session")))

(defun switch-to-tmux (&optional arg)
  "Switch to tmux and change current directory to current
`default-directory' if ARG is non-nil."
  (interactive "P")
  (if (tmux-has-session)
      (apply #'start-process "Attach" nil
             (split-string  (format "urxvt -title %s@%s -e tmux attach-session -d"
                                    (user-login-name) (system-name))))
    (apply #'start-process "Start" nil
           (split-string  (format "urxvt -title %s@%s -e tmux new-session"
                                  (user-login-name) (system-name)))))
  (if arg
      (let ((current-command (tmux-display "#{pane_current_command}")))
        (cond ((string= current-command "R")
               (let ((coding-system-for-write 'utf-8))
                 (shell-command (format "tmux send \"setwd(\\\"%s\\\")\" ENTER"
                                        (file-truename default-directory)))))
              ((string-match "python[23]?" current-command)
               (let ((coding-system-for-write 'utf-8))
                 (shell-command (format "tmux send \"import os; os.chdir(\\\"%s\\\")\" ENTER"
                                        (file-truename default-directory)))))
              (t (let ((coding-system-for-write 'utf-8))
                   (shell-command (format "tmux send \"cd \\\"%s\\\"\" ENTER"
                                          (file-truename default-directory))))))))
  (message "Switched to tmux"))

(defun switch-to-tmux-or-suspend (&optional arg)
  "Switch to tmux if in a graphic session. Otherwise, suspend emacs.
Change directory to `default-directory' if ARG is non-nil."
  (interactive "P")
  (if (display-graphic-p)
      (switch-to-tmux arg)
    (suspend-emacs (if arg (format "cd \"%s\"" (file-truename default-directory))))))

(global-set-key (kbd "C-z") 'switch-to-tmux-or-suspend)

;; Notify events
(with-emacs-version>= "24"
  (require 'notifications))

(require 'ffap)
;; Find file at point even if it is the wrong user: /home/otheruser/.bashrc
(add-to-list 'ffap-alist
             (cons "\\`\\(/home/[^/]+\\)"
                   (lambda (name)
                     (replace-match "~" nil nil name 1))))

;; Find pdf at a ref which has the same name in `pdfs-directory'
(defvar pdfs-directory nil
  "Directory to look for pdf files.")

(put 'pdfs-directory 'safe-local-variable 'string-or-null-p)

(defun ffap-bib-latex-mode (bib_id)
  "Infer a filename from BIB_ID."
  (when (eq major-mode 'latex-mode)
    (if (and pdfs-directory (file-exists-p (concat pdfs-directory bib_id ".pdf")))
        (concat pdfs-directory name ".pdf")
      (condition-case nil
          (concat (file-name-directory (car (reftex-get-bibfile-list))) bib_id ".pdf")
        (error nil)))))

(add-to-list 'ffap-alist '(latex-mode . ffap-bib-latex-mode))
(add-to-list 'ffap-alist '(org-mode . ffap-bib-latex-mode))

(defun my-find-thing-at-point ()
  "Find variable, function or file at point."
  (interactive)
  (cond ((not (eq (variable-at-point) 0))
         (call-interactively 'describe-variable))
        ((function-called-at-point)
         (call-interactively 'describe-function))
        ((and (thing-at-point 'filename) (file-exists-p (thing-at-point 'filename)))
         (find-file (thing-at-point 'filename)))
        ((thing-at-point 'url)
         (browse-url (thing-at-point 'url)))
        (t
         (find-file-at-point))))

(global-set-key (kbd "C-x C-p") 'my-find-thing-at-point)

;; Using modified version of autoinsert to allow multiple autoinsert
;; https://github.com/thisirs/auto-insert-multiple.git
(add-to-list 'load-path (expand-file-name "auto-insert-multiple" site-lisp-directory))
(require 'autoinsert)
(auto-insert-mode t)

(setq auto-insert-directory (expand-file-name "~/.emacs.d/autoinsert/"))
(setq auto-insert-query nil)

(defun auto-insert-yasnippet-expand (snippet)
  "Expand yasnippet's SNIPPET in current buffer."
  (with-demoted-errors
      (save-window-excursion
        (require 'yasnippet)
        ;; make buffer visible before yasnippet
        ;; which might ask the user for something
        (switch-to-buffer (current-buffer))
        (yas-expand-snippet snippet))))

(defmacro auto-insert-add-from-yasnippet (mode key &rest actions)
  (declare (indent defun))
  (mapc (lambda (dir)
          (if (file-directory-p (expand-file-name (symbol-name mode) dir))
              (yas--load-directory-1
               (expand-file-name (symbol-name mode) dir)
               mode)))
        (yas-snippet-dirs))
  (let ((snippets (mapcan #'(lambda (table)
                              (yas--fetch table key))
                          (let ((major-mode mode))
                            (yas--get-snippet-tables)))))
    (if snippets
        (cons 'progn (mapcar
                      (lambda (template)
                        `(push (cons (cons ',mode ,(car template))
                                     '(lambda ()
                                        (auto-insert-yasnippet-expand
                                         ,(yas--template-content (cdr template)))))
                               auto-insert-alist))
                      (mapcan #'(lambda (table)
                                  (yas--fetch table key))
                              (let ((major-mode mode))
                                (yas--get-snippet-tables)))))
      (error "No snippet with key \"%s\" in mode %s" key mode))))

(auto-insert-add-from-yasnippet latex-mode "hdr"
  (TeX-normal-mode 1))
(auto-insert-add-from-yasnippet sh-mode "sb"
  (normal-mode))
(auto-insert-add-from-yasnippet org-mode "hdr")

(setq auto-insert 'other)

(cond ((member "Fira Mono" (font-family-list))
       (set-frame-font "Fira Mono-14" nil t))
      ((member "Hack" (font-family-list))
       (set-frame-font "Hack-14" nil t))
      ((member "Cousine" (font-family-list))
       (set-frame-font "Cousine-14" nil t))
      ((member "Inconsolata" (font-family-list))
       (set-frame-font "Inconsolata-14" nil t)))

;; UTF-8 as default encoding
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8-unix)
(setq keyboard-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)

;; Unified diff format and no whitespace when using `diff'
(setq diff-switches "-u -w")

;; No limit on how many lines to keep in *Messages* buffer
(setq message-log-max t)

;; No fringe on the right
(set-fringe-mode '(8 . 0))

;; No fringe in minibuffer
(set-window-fringes (minibuffer-window) 8 0)

;; Don't automatically split vertically
(setq split-height-threshold nil)

;; Clashes with org's date selection in calendar
;; Quit minibuffer if there is a click on another buffer
;; (defun stop-using-minibuffer ()
;;   "kill the minibuffer"
;;   (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
;;     (abort-recursive-edit)))

;; (add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)


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

;; Bar cursor
(setq-default cursor-type 'box)

;; Don't blink the cursor
(blink-cursor-mode -1)

;; Drive out the mouse when it's too near to the cursor.
(mouse-avoidance-mode 'animate)

;;; Prevent Extraneous Tabs
(setq-default indent-tabs-mode nil)

;; Follow links to version-controlled files
(setq vc-follow-symlinks t)

(defalias 'yes-or-no-p 'y-or-n-p)


;; Leave point at center of the screen when scrolling
(setq scroll-preserve-screen-position t)

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

(defun transpose-buffers (&optional arg)
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
(setq custom-file (make-temp-file "custom" nil ".el"))
(load custom-file 'noerror)

;;; Conditional untabify, delete trailing whitespaces
(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  "Indent the current buffer."
  (interactive)
  (save-restriction (indent-region (point-min) (point-max))))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically
on save."
  (interactive)
  (untabify-buffer)
  (indent-buffer)
  (delete-trailing-whitespace))
(defalias 'clean-buffer 'cleanup-buffer)

(defvar delete-trailing-whitespace-hook nil
  "Hook to cancel deleting trailing whitespaces.")

(defvar untabify-hook nil
  "Hook to cancel untabifying.")

(defun delete-trailing-whitespace-maybe ()
  "If all the hook functions return nil, delete trailing whitespaces."
  (unless (run-hook-with-args-until-success 'delete-trailing-whitespace-hook)
    (delete-trailing-whitespace)))

(defun shared-on-vc ()
  "Return non-nil if current buffer is editing an already checked
in file in a non-autocommitted repository."
  (and (buffer-file-name)
       (memq (vc-backend (buffer-file-name)) vc-handled-backends)
       (not (and (fboundp 'vc-auto-commit-backend)
                 (vc-auto-commit-backend)))))

(defvar shared-directory-list
  '("~/Dropbox/Documents-sy09/"
    "~/ownCloud/Shared/")
  "Shared directories")

(defun shared-directory ()
  "Return non-nil if current buffer is visiting a file that is
shared as specified in `shared-directory-list'."
  (when-let* ((fname (buffer-file-name)))
    (seq-some (lambda (e)
                (string-prefix-p
                 (directory-file-name (expand-file-name e))
                 (directory-file-name (expand-file-name fname))))
              shared-directory-list)))

;; Don't delete trailing whitespaces on checked in vc-controlled files
;; that are not auto-committed and on shared files
(add-hook 'delete-trailing-whitespace-hook #'shared-on-vc)
(add-hook 'delete-trailing-whitespace-hook #'shared-directory)

(defun untabify-vc ()
  "Return non-nil if tabs should remain."
  (or indent-tabs-mode (shared-on-vc)))

(defun untabify-shared ()
  (or indent-tabs-mode (shared-directory)))

;; Don't untabify (1) vc-controlled checked in files that are not
;; auto-committed, (2) shared files through ownCloud and (3) buffer
;; where `indent-tabs-mode' is on.
(add-hook 'untabify-hook #'untabify-vc)
(add-hook 'untabify-hook #'untabify-shared)

(defun untabify-maybe ()
  "Untabify the current buffer but give `untabify-hook' a chance
to cancel it."
  (unless (run-hook-with-args-until-success 'untabify-hook)
    (untabify-buffer)))

(defun cleanup-buffer-maybe ()
  (interactive)
  (untabify-maybe)
  (delete-trailing-whitespace-maybe))

(add-hook 'before-save-hook 'cleanup-buffer-maybe)

;; Bookmarks
(setq bookmark-default-file "~/CloudStation/Sylvain/emacs/.bookmarks")

;; Save bookmarks every time
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

;; Ignore case when completing
(setq completion-ignore-case t)

;; Filenames too, to browse with dired for example...
(setq read-file-name-completion-ignore-case t)

(defun change-to-utf-8 ()
  (interactive)
  (set-buffer-file-coding-system 'utf-8-unix))

(global-set-key [(f2)] 'change-to-utf-8)

(setq set-mark-command-repeat-pop t)

;; Minibuffer history
(savehist-mode t)
(setq savehist-additional-variables
      ;; also save my search entries
      '(search-ring regexp-search-ring)
      savehist-file "~/.emacs.d/savehist")

;; Always add a final newline
(setq require-final-newline t)

;; Display time in 24 hours format
(setq display-time-24hr-format t)

;; Display time in the modeline
(display-time-mode t)

;; Do not display the load
(setq display-time-default-load-average nil)

(setq sentence-end-double-space nil)

(setq select-enable-clipboard t)

;; Default in 24.4
;; (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; Save clipboard strings into kill ring before replacing them
(setq save-interprogram-paste-before-kill t)

(if (version<= "26" emacs-version)
    (setq confirm-kill-processes nil)

  ;; Don't warn when quitting emacs with running processes
  (defun no-processes (oldfun &optional arg)
    "Prevent annoying \"Active processes exist\" query when you quit Emacs."
    (letf (((symbol-function 'process-list) (lambda ())))
      (funcall oldfun arg)))
  (advice-add 'save-buffers-kill-emacs :around #'no-processes))

;; Don't warn when killing running processes
(delq 'process-kill-buffer-query-function
      kill-buffer-query-functions)

;; Make URLs/mail adresses in comments/strings highlighted and clickable
(add-hook 'find-file-hook 'goto-address-prog-mode)

;; Enable narrow-to-region binding
(put 'narrow-to-region 'disabled nil)

;; Delete the selected region when something is yanked, typed or with
;; DEL
(delete-selection-mode t)

;; Text selection highlighted by default on Emacs 23
;;(transient-mark-mode t)

;; Non interactive function in apropos
;; Make C-h a act as C-u C-h a
(setq apropos-do-all t)

;; Use system trash (for emacs 23)
(setq delete-by-moving-to-trash t)

;; Activate automatic update of in-file timestamp
(setq time-stamp-active t)

;; Time-stamp format like this: <2014-05-03 18:44:06 (thisirs)>
(setq time-stamp-format "%04y-%02m-%02d %02H:%02M:%02S (%u)")

(defun time-stamp-insert ()
  "Insert a commented out time-stamp line."
  (interactive)
  (insert (if comment-start
              (concat comment-start " ")
            "")
          (format "Time-stamp: <%s>" (time-stamp-string))))

(defalias 'insert-time-stamp 'time-stamp-insert)

(add-hook 'before-save-hook 'time-stamp)

;; Make scripts executable on save
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Open bash-fc-* files from fc command or C-x C-e in terminal in sh-mode
(add-to-list 'auto-mode-alist '("bash-fc-[0-9]+\\'" . sh-mode))

;; Taken from http://emacs.stackexchange.com/questions/519/key-bindings-specific-to-a-buffer
(defvar temp-mode-map (make-sparse-keymap)
  "Keymap while temp-mode is active.")

(define-minor-mode temp-mode
  "A temporary minor mode to be activated only specific to a buffer."
  nil
  :lighter " Temp"
  temp-mode-map)

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

(defun kill-emacs-or-frame (&optional arg)
  "Kill emacs, close frame or editing buffer.

If a server buffer is current, it is marked \"done\" and
optionnaly saved. Otherwise, kill current frame if there
is more than one or kill emacs if there is only one."
  (interactive "P")
  (cond
   ((and (boundp 'server-buffer-clients) server-buffer-clients)
    (apply 'server-switch-buffer (server-done)))
   ((> (length (visible-frame-list)) 1)
    (delete-frame))
   (t (save-buffers-kill-emacs arg))))

(global-set-key "\C-x\C-c" 'kill-emacs-or-frame)

;; Lower emacs frame when done editing; make an exception for commit
;; messages as magit "calls himself" to edit commit messages.
(add-hook 'server-done-hook 'lower-frame-unless-commit)

(defun lower-frame-unless-commit ()
  (unless (and (boundp 'with-editor-mode) with-editor-mode)
    (lower-frame)))

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

(defun reb-read-regexp (prompt &optional defaults history)
  "Like `read-regexp' but with `re-builder' feedback."
  (let ((reb-re-syntax 'string))
    (re-builder)
    (recursive-edit))
  (let ((re (with-output-to-string
              (print (reb-target-binding
                      (prog1 reb-regexp
                        (reb-quit)))))))
    (read re)))

(defadvice reb-quit (after recursive-quit activate)
  (if (> (recursion-depth) 0)
      (exit-recursive-edit)))

(defun collect-regexp (regexp &optional beg end)
  "Collect all string matched by REGEXP and store it in the kill
ring."
  (interactive (cons (reb-read-regexp "Collect regexp: ")
                     (if (use-region-p)
                         (list (region-beginning) (region-end))
                       (list (point) nil))))
  (save-excursion
    (goto-char beg)
    (let (acc)
      (while (re-search-forward regexp end t)
        (push (match-string 0) acc))
      (let ((print-length 10))
        (message "Kill-ring: %s" acc))
      (kill-new (mapconcat 'identity (nreverse acc) "\n")))))

;;; init.el ends here


