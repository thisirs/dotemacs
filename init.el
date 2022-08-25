;; -*- lexical-binding: t; -*-
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-utils)

(defvar personal-directory "~/SynologyDrive/Sylvain/")
(defvar projects-directory "~/SynologyDrive/Sylvain/projects")

;; Add personal site-lisp to load-path
(defvar personal-emacs-directory "~/SynologyDrive/Sylvain/emacs/")

(defvar site-lisp-directory (expand-file-name "site-lisp" personal-emacs-directory ))

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

(when (display-graphic-p)
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

(setq ring-bell-function #'ignore)

;; Backups
(setq make-backup-files t
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

;; Auto-save remote file in /tmp and the rest in
;; ~/.emacs.d/var/auto-save/files/
(add-to-list 'auto-save-file-name-transforms
             '(".*" "~/.emacs.d/var/auto-save/files/" t) t)

(setq auto-save-visited-interval 60)
(auto-save-visited-mode +1)

;; No lockfiles
(setq create-lockfiles nil)

(mouse-wheel-mode 1)

(setq straight-recipes-gnu-elpa-use-mirror t)
(setq straight-repository-branch "develop")
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)
(setq use-package-verbose nil)
;; (setq use-package-verbose 'debug)
;; (setq debug-on-error t)
(setq use-package-minimum-reported-time 0.1)
(setq use-package-always-defer t)
(setq use-package-hook-name-suffix "")
;; (setq use-package-verbose 'debug)
(straight-use-package 'use-package)
(straight-use-package 'diminish)
(require 'use-package)
(setq use-package-always-defer t)
(setq use-package-compute-statistics t)

(require 'bind-key)

;; https://github.com/emacscollective/no-littering
(use-package no-littering               ; help keeping ~/.emacs.d clean
  :demand
  :preface
  (defun change-base-dir (file)
    (setq file (abbreviate-file-name (expand-file-name file)))
    (if (string-prefix-p user-emacs-directory file)
        (expand-file-name (substring file (length user-emacs-directory)) personal-emacs-directory)
      (user-error "Unable to change base directory")
      file))

  (defun set-no-littering-base-dir (&rest vars)
    (mapc (lambda (var)
            (if (not (boundp var))
                (user-error "Unbounded variable `%s'" var)
              (if (not (featurep 'no-littering))
                  (user-error "Package `no-littering' not loaded")
                (set var (change-base-dir (symbol-value var))))))
          vars)))

(load (expand-file-name "personal.el" personal-emacs-directory) :noerror)

;; No confirmation when loading theme
(setq custom-safe-themes t)

;; Loading zenburn theme
;; http://github.com/bbatsov/zenburn-emacs
(use-package zenburn-theme              ; A low contrast color theme for Emacs.
  :demand
  :if (on-zouzou)
  :if (window-system)
  :config
  (load-theme 'zenburn t))

(use-package server
  :if (window-system)
  :demand
  :config
  (unless (server-running-p server-name)
    (server-start)))

(use-package solarized                  ; The Solarized color theme, ported to Emacs.
  :demand
  :if (or (on-zbook) (on-knuth))
  :if (window-system)
  :straight solarized-theme
  :config
  (setq solarized-use-variable-pitch nil)
  (setq solarized-scale-org-headlines nil)
  (load-theme 'solarized-dark t))

;; https://github.com/nashamri/spacemacs-theme
(use-package spacemacs-theme            ; Color theme with a dark and light versions
  :disabled
  :init
  (load-theme 'spacemacs-dark t))

(require 'init-elisp)
(require 'init-bindings)
(require 'init-editing)
(require 'init-fill)
(require 'init-find-file)
(require 'init-latex)

(require 'init-auctex)
(require 'init-desktop)
(require 'init-dired)
(require 'init-erc)
(require 'init-hippie-expand)
(require 'init-ibuffer)
(require 'init-isearch)
(require 'init-org)
(require 'init-scratch)
(require 'init-wcheck)
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
  :straight nil
  :init
  ;; Silently save abbrevs on quitting emacs
  (setq save-abbrevs 'silently))

;; https://github.com/nashamri/academic-phrases
(use-package academic-phrases)          ; Bypass that mental block when writing your papers.

;; https://github.com/minad/affe
(use-package affe                       ; Asynchronous Fuzzy Finder for Emacs
  :disabled
  :demand :after orderless
  :bind ("M-g f" . affe-grep)
  :custom (affe-count 100)
  :config
  ;; Configure Orderless
  (setq affe-regexp-function #'orderless-pattern-compiler
        affe-highlight-function #'orderless--highlight)

  ;; Manual preview key for `affe-grep'
  (consult-customize affe-grep :preview-key (kbd "C-o")))

;; https://github.com/Wilfred/ag.el
(use-package ag                         ; A front-end for ag ('the silver searcher'), the C ack replacement.
  :disabled
  :if (and (executable-find "ag")
           (not (executable-find "rg")))
  :bind ("M-g f" . ag-search-current-directory)
  :config
  ;; http://github.com/mhayashi1120/Emacs-wgrep/raw/master/wgrep-ag.el
  (use-package wgrep-ag                 ; Writable ag buffer and apply the changes to files
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
  :config
  (alert-add-rule :style 'libnotify))

;; https://github.com/syohex/emacs-anzu
(use-package anzu                       ; Display incremental search stats in the modeline.
  :disabled
  :config
  (global-anzu-mode 1)
  :diminish anzu-mode)

(use-package app-launcher
  :straight '(app-launcher :host github :repo "SebastienWae/app-launcher")
  :preface
  (defun app-launcher--action-function-file (selected &optional file)
    "Open `file' with the selected application."
    (let* ((exec (cdr (assq 'exec (gethash selected app-launcher--cache))))
           (command (mapconcat
                     (lambda (chunk)
                       (cond
                        ((or (equal chunk "%U")
                             (equal chunk "%F")
                             (equal chunk "%u")
                             (equal chunk "%f"))
                         (if file (shell-quote-argument file) ""))
                        (t chunk)))
                     (split-string exec)
                     " ")))
      (call-process-shell-command command nil 0 nil)))
  :custom (app-launcher--action-function #'app-launcher--action-function-file))

;; http://nschum.de/src/emacs/auto-dictionary/
(use-package auto-dictionary            ; automatic dictionary switcher for flyspell
  :bind (("C-c w l" . adict-change-dictionary)
         ("C-c w g" . adict-guess-dictionary))
  :init
  (add-hook 'flyspell-mode-hook #'auto-dictionary-mode))

;; https://github.com/alpha22jp/atomic-chrome
(use-package atomic-chrome              ; Edit Chrome text area with Emacs using Atomic Chrome
  :demand
  :config
  (atomic-chrome-start-server))

;; https://github.com/abo-abo/avy
(use-package avy                        ; tree-based completion
  :config
  (setq avy-style 'at)
  (setq avy-keys '(?a ?z ?e ?r ?t ?y ?u ?i ?o ?p
                      ?q ?s ?d ?f ?g ?h ?j ?k ?l ?m
                      ?w ?x ?c ?v ?b ?n))
  (setq avy-background t)
  :bind ("M-h" . avy-goto-subword-1))

;; https://github.com/DamienCassou/beginend
(use-package beginend                   ; Redefine M-< and M-> for some modes
  :demand
  :diminish beginend-global-mode
  :config
  (dolist (mode beginend-modes) (diminish (cdr mode)))
  (beginend-global-mode))

;; https://github.com/bdarcus/citar
(use-package citar             ; Citation-related commands for org, latex, markdown.
  :bind (("C-c b" . citar-open-or-cite)
         :map citar-map
         ;; Allow to open resources with selected application
         ("O" . citar-open-external))
  :init
  (defun citar-open-or-cite ()
    (require 'citar)
    (interactive)
    (call-interactively
     (if (citar--get-major-mode-function 'insert-citation)
         'citar-insert-citation 'citar-open)))
  :custom
  (org-cite-global-bibliography (list (expand-file-name "recherche/biblio/refs.bib" personal-directory)))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  (citar-library-paths (list (expand-file-name "recherche/biblio" personal-directory)))
  ;; Org-roam notes
  (citar-notes-paths (list (expand-file-name "recherche/notes" personal-directory)))
  :config
  ;; https://github.com/emacs-citar/citar-embark
  (use-package citar-embark             ; Citar/Embark integration
    :straight nil
    :demand :after citar embark
    :no-require
    :config (citar-embark-mode))

  ;; https://github.com/domtronn/all-the-icons.el
  (use-package all-the-icons            ; A library for inserting Developer icons
    :config
    (setq citar-symbols
          `((file ,(all-the-icons-faicon "file-o" :face 'all-the-icons-green :v-adjust -0.1) . " ")
            (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) . " ")
            (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01) . " ")))
    (setq citar-symbol-separator "  "))

  ;; Don't display link
  (setf (cdr (assoc 'link citar-symbols)) (cons "" ""))

  (defun orb-citar-edit-note-template (citekey _entry)
    "Use `org-roam-bibtex' to open a note file.

This function is used in `citar-open-note-function'."
    (require 'org-roam-bibtex)
    (if-let ((tmpl (assoc "r" org-roam-capture-templates)))
      (let ((org-roam-capture-templates (list tmpl)))
        (orb-edit-note citekey))
      (error "No template with key `r' found in `org-roam-capture-templates'")))

  (setq citar-open-note-function 'orb-citar-edit-note-template)

  (defun citar-open-external (key-or-keys)
    (citar--library-file-action key-or-keys #'citar-file-open-external))

  (defun citar-file-open-external (file)
    "Select application with `app-launcher' and open `file' with it."
    (require 'app-launcher)
    (let* ((candidates (app-launcher-list-apps))
           (result (completing-read
                    "Run app: "
                    (lambda (str pred flag)
                      (if (eq flag 'metadata)
                          '(metadata
                            (annotation-function . (lambda (choice)
                                                     (funcall
                                                      app-launcher--annotation-function
                                                      choice))))
                        (complete-with-action flag candidates str pred)))
                    (lambda (x y) (cdr (assq 'visible y)))
                    t nil 'app-launcher nil nil)))
      (funcall app-launcher--action-function result file))))

;; https://github.com/proofit404/blacken
(use-package blacken                    ; Reformat python buffers using the "black" formatter
  :disabled t)

(use-package bookmark
  :straight nil
  :custom
  (bookmark-set-fringe-mark nil)
  (bookmark-default-file (change-base-dir bookmark-default-file))
  (bookmark-watch-bookmark-file 'silent)
  (bookmark-save-flag 1)
  :preface
  ;; Support for placeholder in filename of bookmarks
  (defun bookmark-get-filename-advice (bookmark-name-or-record)
    (if-let ((spec (bookmark-prop-get bookmark-name-or-record 'spec))
             (filename (bookmark-prop-get bookmark-name-or-record 'filename)))
        (progn (if (symbolp spec)
                   (setq spec (funcall spec)))
               (format-spec filename spec))
      (bookmark-prop-get bookmark-name-or-record 'filename)))

  (advice-add #'bookmark-get-filename :override #'bookmark-get-filename-advice)

  (defun bookmark-spec ()
    `((?a . ,(UTC-autumn-from-time (current-time)))
      (?p . ,(UTC-spring-from-time (current-time)))
      (?s . ,(UTC-semester-from-time (current-time)))))

  ;; Support for dynamically generated bookmarks
  (defun bookmark-not-generated (bmk-record)
    (null (bookmark-prop-get bmk-record 'generated)))

  (defun bookmark-save-filter (oldfun &optional parg file make-default)
    "Don't save generated bookmarks"
    (let ((bookmark-alist (seq-filter #'bookmark-not-generated bookmark-alist)))
      (funcall oldfun parg file make-default)))

  (advice-add 'bookmark-save :around #'bookmark-save-filter)

  (defun bookmark-add-generated-bookmarks (file &optional overwrite no-msg default)
    "Import bookmarks generated from specific directories."
    (let ((directory "~/SynologyDrive/Sylvain/enseignements/"))
      (bookmark-import-new-list
       (mapcar (lambda (dir)
                 `(,dir
                   (filename . ,(expand-file-name dir directory))
                   (position . 0)
                   (generated . t)))
               (directory-files directory nil "^\\(A\\|P\\)[0-9]\\{4\\}"))))
    (let ((directory "~/SynologyDrive/Sylvain/Documents/"))
      (bookmark-import-new-list
       (mapcar (lambda (dir)
                 `(,dir
                   (filename . ,(expand-file-name dir directory))
                   (position . 0)
                   (generated . t)))
               (directory-files directory nil "^[0-9]\\{4\\}")))))

  (advice-add 'bookmark-load :after #'bookmark-add-generated-bookmarks))

(use-package cmake-mode)        ; major-mode for editing CMake sources

;; http://company-mode.github.io/
(use-package company                    ; Modular text completion framework
  :disabled
  :diminish company-mode
  :defines
  (company-dabbrev-ignore-case company-dabbrev-downcase)
  :bind
  (:map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("<tab>" . company-complete-common-or-cycle)
        :map company-search-map
        ("C-p" . company-select-previous)
        ("C-n" . company-select-next))
  :custom
  (company-idle-delay 0)
  (company-echo-delay 0)
  (company-minimum-prefix-length 1)
  :hook
  (after-init-hook . global-company-mode))

;; https://github.com/tumashu/company-posframe
(use-package company-posframe           ; Use a posframe as company candidate menu
  :disabled
  :custom (company-posframe-show-indicator t)
  :hook (company-mode-hook . company-posframe-mode))

(use-package compile
  :custom
  ;; Move point to first error
  (compilation-scroll-output 'first-error))

;; https://github.com/minad/consult
(use-package consult                    ; Consulting completing-read
  :init
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :bind (([remap list-buffers] . consult-buffer)
         ("C-x l" . consult-locate)
         ("M-g i" . consult-imenu)
         ("M-g f" . consult-find)
         ("M-g m" . consult-mark)
         ("M-g g" . consult-grep)
         ("M-g l" . consult-goto-line)

         ([remap bookmark-jump] . consult-bookmark)
         ([remap yank-pop] . consult-yank-pop)
         ([remap keep-lines] . consult-keep-lines))
  :custom
  (completion-in-region-function #'consult-completion-in-region))

;; https://github.com/karthink/consult-dir
(use-package consult-dir                ; Consult based directory picker
  :demand :after vertico
  :bind
  (:map vertico-map
        ("M-." . consult-dir)
        ("M-j" . consult-dir-jump-file)))

;; https://github.com/jgru/consult-org-roam
(use-package consult-org-roam           ; Consult integration for org-roam
  :diminish
  :demand :after org-roam
  :bind
  ("C-c n s" . consult-org-roam-search)
  :custom
  (consult-org-roam-grep-func #'consult-ripgrep)
  :config
  (consult-org-roam-mode 1)
  ;; Eventually suppress previewing for certain functions
  (consult-customize
   consult-org-roam-forward-links
   :preview-key (kbd "M-.")))

;; https://github.com/minad/corfu
(use-package corfu                      ; Completion Overlay Region FUnction
  :demand
  :custom
  (corfu-cycle t)
  :config
  (global-corfu-mode))

;; https://github.com/abo-abo/swiper
(use-package counsel                    ; Various completion functions using Ivy
  :disabled
  :demand
  :bind (("C-x l" . counsel-locate)
         :map counsel-find-file-map
         ("<right>" . counsel-down-directory)
         ("<left>" . counsel-up-directory)
         :map ivy-minibuffer-map
         ("M-y" . ivy-next-line))
  :custom ((counsel-yank-pop-separator
            "\n────────────────────────────────────────────────────────\n"))
  :config
  ;; Remaps some built-in emacs functions
  (counsel-mode))

;; https://github.com/raxod502/ctrlf
(use-package ctrlf                      ; Emacs finally learns how to ctrl+F
  :demand
  :init
  (defun ctrlf-yank-word-or-char ()
    (interactive)
    (let ((input (field-string (point-max))) yank)
      (when (or ctrlf--match-bounds (= (length input) 0))
        (with-current-buffer (window-buffer (minibuffer-selected-window))
          (setq yank (buffer-substring-no-properties
                      (or (and ctrlf--match-bounds
                               (cdr ctrlf--match-bounds))
                          ctrlf--current-starting-point)
                      (progn (forward-word) (point)))))
        (goto-char (field-end (point-max)))
        (insert yank))))
  :bind (:map ctrlf-minibuffer-mode-map ("C-w" . ctrlf-yank-word-or-char))
  :config
  (ctrlf-mode +1))

;; http://github.com/rejeep/drag-stuff
(use-package drag-stuff                 ; Drag stuff (lines, words, region, etc...) around
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
(use-package dockerfile-mode)   ; Major mode for editing Docker's Dockerfiles

;; https://github.com/jacktasia/dumb-jump
(use-package dumb-jump                  ; jump to definition for multiple languages without configuration.
  :after xref
  :hook (xref-backend-functions . dumb-jump-xref-activate)
  :custom
  (dumb-jump-prefer-searcher 'rg))

;; ediff settings
(use-package ediff-diff
  :straight nil
  :custom
  ;; Ignore whitespace in diff
  (ediff-diff-options "-w"))

(use-package ediff-wind
  :straight nil
  :preface
  (defalias 'ediff-buffer-with-file 'ediff-current-file)

  ;; Show all in org files with ediff
  (defun ediff-outline-show-all ()
    (if (eq major-mode 'org-mode)
        (outline-show-all)))
  :hook
  (ediff-prepare-buffer-hook . ediff-outline-show-all)

  ;; Restore window configuration after quit
  (ediff-before-setup-hook . (lambda () (window-configuration-to-register 'ediff)))
  (ediff-quit-hook . (lambda () (jump-to-register 'ediff)))
  :custom
  ;; No separate frame for ediff control buffer
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package electric
  :straight nil
  :demand
  :config
  (electric-indent-mode 1))

;; https://github.com/davidshepherd7/electric-operator
(use-package electric-operator          ; Automatically add spaces around operators
  :after ess
  :hook ((ess-r-mode-hook inferior-ess-r-mode-hook) . electric-operator-mode)
  :custom
  (electric-operator-R-named-argument-style 'spaced))

(use-package elec-pair
  :if (or (on-zbook) (on-knuth))
  :straight nil
  :demand
  :config
  (electric-pair-mode))

;; https://github.com/skeeto/elfeed
(use-package elfeed                     ; an Emacs Atom/RSS feed reader
  :custom
  (elfeed-search-title-max-width 120)
  (elfeed-db-directory (change-base-dir elfeed-db-directory))
  (elfeed-enclosure-default-dir (change-base-dir elfeed-enclosure-default-dir)))

;; https://github.com/remyhonig/elfeed-org
(use-package elfeed-org                 ; Configure elfeed with one or more org-mode files
  :demand :after (:all org elfeed)
  :custom
  (rmh-elfeed-org-files (list (expand-file-name "elfeed.org" personal-emacs-directory)))
  :config
  (elfeed-org))

;; https://github.com/sp1ff/elfeed-score
(use-package elfeed-score               ; Gnus-style scoring for Elfeed
  :demand :after elfeed
  :custom
  (elfeed-score-serde-score-file (change-base-dir elfeed-score-serde-score-file))
  :config
  (setq elfeed-score-serde-score-file (change-base-dir elfeed-score-serde-score-file))
  (define-key elfeed-search-mode-map "=" elfeed-score-map)
  (elfeed-score-enable))

;; https://github.com/jorgenschaefer/elpy
(use-package elpy                       ; Emacs Python Development Environment
  :demand :after python
  :bind (:map python-mode-map ("C-c C-c" . elpy-shell-send-group-and-step-or-region))
  :config
  (defun elpy-send-region-or-buffer-and-step ()
    (interactive "P")
    (if (use-region-p)
        (elpy-shell--flash-and-message-region (region-beginning) (region-end))
      (elpy-shell--flash-and-message-region (point-min) (point-max)))
    (elpy-shell--send-region-or-buffer-internal)
    (if (use-region-p)
        (goto-char (region-end))
      (goto-char (point-max))))

  (defun elpy-shell-send-group-and-step-or-region (&optional go)
    (interactive)
    (if (region-active-p)
        (elpy-shell-send-region-or-buffer-and-step)
      (elpy-shell-send-group-and-step))))

;; https://github.com/oantolin/embark
(use-package embark                     ; Conveniently act on minibuffer completions
  :after vertico
  :bind
  (("C-x C-p" . embark-act)
   (:map vertico-map
         ("C-c C-o" . embark-export)
         ("C-c C-c" . embark-act)))
  :custom
  (embark-prompter #'embark-completing-read-prompter)
  (embark-indicators '(embark-minimal-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; https://github.com/oantolin/embark
(use-package embark-consult             ; Consult integration for Embark
  :demand :after (embark consult)
  :hook
  (embark-collect-mode-hook . embark-consult-preview-minor-mode))

;; https://github.com/hrs/engine-mode
(use-package engine-mode                ; Define and query search engines from within Emacs.
  :disabled
  :bind* ("C-c /" . engine-mode-hydra/body)
  :config
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

(use-package epwdgen                    ; Flexible password generator
  :straight `(epwdgen :type git
                     :local-repo ,(expand-file-name "epwdgen" projects-directory))
  :commands epwdgen-generate-password
  :config
  (setq epwdgen-password-presets
        '(("passphrase, 4 words, space separator" passphrase
           :sep " " :file "/home/sylvain/SynologyDrive/Sylvain/wordlist.lst")
          ("alphanumeric, length 10" password
           :length 10
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
  :defer 10
  :bind ("M-:" . eval-expr)
  :custom
  (eval-expr-print-function 'pp)
  (eval-expr-print-level 20)
  (eval-expr-print-length 100)
  :config
  ;; Enable paredit and eldoc in eval-expr
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
  :defer 10
  :custom
  ;; Don't run a login shell
  (exec-path-from-shell-arguments '("-l"))
  :config
  (setq exec-path-from-shell-variables
        (append exec-path-from-shell-variables
                (list "SSH_AGENT_PID" "SSH_AUTH_SOCK")))
  (exec-path-from-shell-initialize))

;; https://github.com/magnars/expand-region.el
(use-package expand-region              ; Increase selected region by semantic units.
  :bind (("C-à" . er/expand-region)
         ("C-M-à" . er/contract-region)))

;; Open quickly a temporary file
;; https://github.com/thisirs/find-temp-file.git
(use-package find-temp-file             ; Open quickly a temporary file
  :hook
  (after-init-hook . set-scratch-buffer-default-directory)
  ;; Save scratch buffer as temp file when quitting emacs
  (kill-emacs-hook . find-temp-file-save-scratch)
  :bind ("C-x C-t" . find-temp-file)
  :commands find-temp-file--filename
  :custom
  (find-temp-file-directory "~/SynologyDrive/Sylvain/drafts")

  ;; Path is: <mode name>/<date>/<prefix>-<sha1>.<ext>
  (find-temp-template-default "%M/%D/%N-%T.%E")

  :init
  (defun set-scratch-buffer-default-directory ()
    (require 'find-temp-file)
    (with-current-buffer "*scratch*"
      (setq default-directory (expand-file-name "emacs-lisp-mode" find-temp-file-directory))))

  (defun find-temp-file-save-scratch ()
    "Save *scratch* buffer as a draft file."
    (interactive)
    (if (and (get-buffer "*scratch*")
             (with-current-buffer "*scratch*"
               (buffer-modified-p)))
        (with-temp-buffer
          (insert-buffer-substring "*scratch*")
          (let* ((find-temp-template-alist (list (cons "el" "%M/%D/%N-scratch-%T.el")))
                 (file-path (find-temp-file--filename "el")))
            (make-directory (file-name-directory file-path) :parents)
            (write-file file-path)))))
  :config
  ;; Change template for Python and Matlab files (no dash in filename)
  (add-to-list 'find-temp-template-alist (cons "py" "%M/%D/%N_%T.%E"))
  (add-to-list 'find-temp-template-alist (cons "m" "%M/%D/%N_%T.%E")))

;; https://depp.brause.cc/firestarter
(use-package firestarter                ; Execute (shell) commands on save
  :hook (prog-mode-hook . firestarter-mode)
  :init
  (setq firestarter-default-type 'failure))

;; http://www.flycheck.org
(use-package flycheck                   ; On-the-fly syntax checking
  :commands global-flycheck-mode
  :defer 10
  :config
  (global-flycheck-mode 1)
  (setq-default flycheck-disabled-checkers
                '(emacs-lisp emacs-lisp-checkdoc tex-chktex tex-lacheck))

  (setq flycheck-lintr-linters "with_defaults(commented_code_linter = NULL, line_length_linter = line_length_linter(120))"))

;; https://github.com/flycheck/flycheck-color-mode-line
(use-package flycheck-color-mode-line ; Change mode line color with Flycheck status
  :after flycheck
  :hook (flycheck-mode . flycheck-color-mode-line-mode)
  :config
  (setq flycheck-highlighting-mode 'symbols))

(use-package folding
  :init
  (defun unfold-current ()
    (interactive)
    (if (equal (point) (car-safe (folding-skip-folds 'backward)))
        (folding-toggle-show-hide)
      (let* ((folding-mode nil)
             (original-func (key-binding (kbd "TAB"))))
        (call-interactively original-func))))
  :bind (:map folding-mode-map
              ("TAB" . unfold-current))
  :config
  (folding-add-to-marks-list 'python-mode "# <answer>" "# </answer>"))

;; https://github.com/magit/forge
(use-package forge                      ; Access Git forges from Magit.
  :demand :after magit
  :custom
  (forge-owned-accounts '(("thisirs" :remote-name "fork"))))

;; https://github.com/magit/git-modes
(use-package git-modes)                 ; Major modes for editing Git configuration files

;; https://gitlab.com/pidu/git-timemachine
(use-package git-timemachine            ; Walk through git revisions of a file
  :bind ("C-x v t" . git-timemachine-toggle)
  :commands git-timemachine)

;; https://github.com/atykhonov/google-translate
(use-package google-translate ; Emacs interface to Google Translate.
  :config
  (setq google-translate-translation-directions-alist
        '(("en" . "fr") ("fr" . "en")))
  (setq google-translate-listen-program
        (or (executable-find "mplayer")
            (executable-find "vlc")))
  (defun google-translate--search-tkk ()
    "Search TKK."
    (list 430675 2721866130))
  :bind ("C-c t" . google-translate-smooth-translate))

(use-package grep
  :bind (:map grep-mode-map
              ("a" . grep-toggle-binary-search))
  :config
  (if (and (not (executable-find "ag"))
           (not (executable-find "rg")))
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
  :disabled
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

;; https://github.com/Wilfred/helpful
(use-package helpful                    ; A better *help* buffer
  :bind* (("C-c C-d" . helpful-at-point))
  :bind* (;; Remap standard commands.
          ("C-c C-d" . helpful-at-point)
          ([remap describe-function] . helpful-callable)
          ([remap describe-variable] . helpful-variable)
          ([remap describe-symbol]   . helpful-symbol)
          ([remap describe-key]      . helpful-key)

          ;; Suggested bindings from the documentation at
          ;; https://github.com/Wilfred/helpful.

          :map help-map
          ("F" . helpful-function)
          ("M-f" . helpful-macro)
          ("C" . helpful-command)))

(use-package hl-line
  :disabled
  :config
  ;; Highlight the line only in the active window
  (setq global-hl-line-sticky-flag t)
  (setq hl-line-sticky-flag t)

  ;; hl-line+
  ;; http://www.emacswiki.org/emacs/hl-line+.el
  (use-package hl-line+               ; Extensions to hl-line.el.
    :config
    (toggle-hl-line-when-idle 1) ; Highlight line only when idle
    ;; Number of seconds of idle time after when the line should be highlighted
    (setq hl-line-idle-interval 5)
    ;; Number of seconds for `hl-line-flash' to highlight the line
    (setq hl-line-flash-show-period 3)))

;; https://github.com/abo-abo/hydra
(use-package hydra                      ; Make bindings that stick around.
  :commands jump-hydra-body
  :config
  (defhydra jump-hydra (:color blue)
    "Jump to bookmarks"
    ("g" (progn
           (let ((search (read-string "Google search: ")))
             (browse-url-firefox
              (format "http://www.google.com/search?q=%s"
                      (url-hexify-string search))))) "Google")
    ("m" (browse-url-firefox (format "https://cas.utc.fr/cas/login?service=%s%%3FauthCAS%%3DCAS"
                                     (url-hexify-string "https://moodle.utc.fr/course/view.php?id=1717"))) "UTC Moodle")
    ("j" ivy-bookmarks "Bookmarks")))

(use-package ical2org
  :commands (ical2org/buffer-to-buffer ical2org/import-to-agenda ical2org/convert-file)
  :custom
  (ical2org/event-format "\
* {SUMMARY} en {LOCATION}
  {TIME}"))

;; https://www.emacswiki.org/emacs/download/info%2b.el
;; (use-package info+)             ; Extensions to `info.el'.

;; http://github.com/nonsequitur/inf-ruby
(use-package inf-ruby)          ; Run a Ruby process in a buffer

;; https://github.com/astoff/isearch-mb
(use-package isearch-mb                 ; Control isearch from the minibuffer
  :disabled
  :demand
  :config
  (add-to-list 'isearch-mb--with-buffer #'isearch-yank-word)
  (define-key isearch-mb-minibuffer-map (kbd "C-w") #'isearch-yank-word))

;; https://github.com/tmalsburg/helm-bibtex
(use-package bibtex-completion          ; A BibTeX backend for completion frameworks
  :disabled
  :demand :after bibtex-completion
  :straight (bibtex-completion :host github :repo "tmalsburg/helm-bibtex"
                               :files ("bibtex-completion.el"))
  :config
  ;; Main Bibtex file automatically exported by Zotero
  (setq bibtex-completion-bibliography (list (expand-file-name "recherche/biblio/refs.bib" personal-directory)))

  ;; Base directory of all pdf files
  (setq bibtex-completion-library-path (expand-file-name "recherche/biblio" personal-directory))

  (setq bibtex-completion-cite-prompt-for-optional-arguments nil)
  (setq bibtex-completion-pdf-field "file"))

;; https://github.com/tmalsburg/helm-bibtex
(use-package ivy-bibtex                 ; A bibliography manager based on Ivy
  :disabled
  :straight (ivy-bibtex :type git :host github :repo "tmalsburg/helm-bibtex"
                        :files ("ivy-bibtex.el")
                        :fork (:host github :repo "thisirs/helm-bibtex"))
  :bind ("C-x b" . ivy-bibtex)
  :config
  (setq bibtex-completion-bibliography
        '("~/SynologyDrive/Sylvain/recherche/biblio/refs.bib"))
  (setq bibtex-completion-library-path
        '("~/SynologyDrive/Sylvain/recherche/biblio/tracking/"
          "~/SynologyDrive/Sylvain/recherche/biblio/compressed_sensing/"
          "~/SynologyDrive/Sylvain/recherche/biblio/hashing/"
          "~/SynologyDrive/Sylvain/recherche/biblio/graphs_and_deep_learning/"
          "~/SynologyDrive/Sylvain/recherche/biblio/NN regularization/"
          "~/SynologyDrive/Sylvain/recherche/biblio/books/"))
  (setq bibtex-completion-cite-prompt-for-optional-arguments nil)
  (setq bibtex-completion-pdf-field "file")

  (setq ivy-re-builders-alist
        '((ivy-bibtex . ivy--regex-ignore-order)
          (t . ivy--regex-plus)))

  ;; Always cite with \cite
  (defun bibtex-completion-format-always-cite (oldfun keys)
    (cl-flet ((completing-read (&rest _) "cite"))
      (funcall oldfun keys)))

  (advice-add 'bibtex-completion-format-citation-cite :around
              #'bibtex-completion-format-always-cite))

;; https://github.com/abo-abo/swiper
(use-package ivy                        ; Incremental Vertical completYon
  :disabled
  :demand
  :diminish (ivy-mode . "")
  :bind (("C-x C-b" . ivy-switch-buffer)
         ("C-x j" . jump-hydra/body)
         ("C-c j" . jump-hydra/body))
  :bind (:map ivy-minibuffer-map ("C-w" . ivy-yank-word)
              :map ivy-switch-buffer-map
              ("C-b" . next-line))
  :config
  (setq ivy-height-alist (cons (cons 'counsel-yank-pop 15) ivy-height-alist))

  ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-use-selectable-prompt t)

  ;; number of result lines to display
  (setq ivy-height 10)

  (ivy-mode)

  ;; Don't sort in org-attach-attach-from
  (add-to-list 'ivy-sort-functions-alist (list 'org-attach-attach-from))

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

(use-package emacs
  :bind ("C-c j" . browse-bookmark)
  :config
  (defun bookmarks-get-candidates ()
    (interactive)
    (let* ((fn "~/.mozilla/firefox/bookmark_list.el")
           (width (1- (frame-width)))
           (p1 40.0)
           (p2 50.0)
           (w1 (floor (/ (* (- width 2) p1) 100)))
           (w2 (floor (/ (* (- width 2) p2) 100)))
           (w3 (- width (+ 2 w1 w2)))
           candidates)
      (when (file-exists-p fn)
        (setq candidates (with-temp-buffer
                           (insert-file-contents fn)
                           (goto-char (point-min))
                           (read (current-buffer))))
        (cl-loop
         for candidate in candidates
         collect
         (let* ((href (cdr (assoc "href" (cdr candidate))))
                (title (cdr (assoc "title" (cdr candidate))))
                (tags (cdr (assoc "tags" (cdr candidate))))
                (candidate-hidden (mapconcat 'identity (list href title tags) " "))
                (candidate-main
                 (concat (truncate-string-to-width (concat title) w1 0 ?\ )
                         " "
                         (truncate-string-to-width (concat href) w2 0 ?\ )
                         " "
                         (truncate-string-to-width (concat tags) w3 0 ?\ ))))
           (cons
            (concat
             (propertize candidate-main) " "
             (propertize candidate-hidden 'invisible t))
            href))))))

  (defun browse-bookmark ()
    (interactive)
    (let ((candidates (bookmarks-get-candidates)) choice)
      (setq choice
            (completing-read
             "Bookmark URLs: "
             (lambda (string predicate action)
               (if (eq action 'metadata)
                   `(metadata
                     (display-sort-function . ,#'identity)
                     (category . bookmark-url))
                 (complete-with-action action candidates string predicate)))))
      (browse-url (cdr (assoc choice candidates))))))

;; https://github.com/raxod502/prescient.el
(use-package ivy-prescient              ; prescient.el + Ivy
  :disabled
  :demand :after prescient
  :config (ivy-prescient-mode))

;; https://github.com/joshwnj/json-mode
(use-package json-mode                  ; json beautifier and more
  :commands json-mode)

;; https://github.com/dzop/emacs-jupyter
(use-package jupyter                    ; Jupyter
  :demand :after org
  :config
  (use-package ob-jupyter
    :straight nil
    :config
    (org-babel-jupyter-override-src-block "python"))
  (org-babel-do-load-languages
   'org-babel-load-languages
   (append org-babel-load-languages
           '((jupyter . t)))))

(use-package keyfreq                    ; track command frequencies
  :config
  (let ((filepath (format (expand-file-name (format ".emacs.%s.keyfreq" (system-name)) personal-emacs-directory))))
    (make-directory (file-name-directory filepath) :parents)
    (setq keyfreq-file filepath))
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;; https://github.com/mhayashi1120/Emacs-langtool
(use-package langtool                   ; Grammar check utility using LanguageTool
  :config
  (cond ((file-exists-p "/usr/lib/jvm/java-8-openjdk/jre/bin/java")
         (setq langtool-java-bin "/usr/lib/jvm/java-8-openjdk/jre/bin/java"))
        ((file-exists-p "/usr/bin/java")
         (setq langtool-java-bin "/usr/bin/java")))
  (setq langtool-java-classpath "/usr/share/languagetool:/usr/share/java/languagetool/*:/tmp/bar/languagetool-4.1/LanguageTool-4.1-stable/")
  (setq langtool-default-language "fr"))

;; http://github.com/fredcamps/lsp-jedi
(use-package lsp-jedi                   ; Lsp client plugin for Python Jedi Language Server
  :demand :after lsp-mode
  :config
  (add-to-list 'lsp-disabled-clients 'pyls)
  (add-to-list 'lsp-enabled-clients 'jedi))

;; https://github.com/emacs-lsp/lsp-mode
(use-package lsp-mode                   ; LSP mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  ;; (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (python-mode-hook . lsp))
  :commands lsp)

;; http://immerrr.github.com/lua-mode
(use-package lua-mode)          ; a major-mode for editing Lua scripts

;; https://github.com/joddie/macrostep
(use-package macrostep                  ; interactive macro expander
  :bind ("C-c e m" . macrostep-expand))

;; https://github.com/magit/magit
(use-package magit                      ; A Git porcelain inside Emacs
  :bind ("C-c i" . magit-status)
  :custom
  ;; Let auth-source handle the passwords for me
  (magit-process-find-password-functions '(magit-process-password-auth-source))
  (magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  :config
  ;; Add local branches section in magit status
  (unless (memq 'magit-insert-local-branches magit-status-sections-hook)
    (setq magit-status-sections-hook
          (seq-insert-at magit-status-sections-hook 'magit-insert-local-branches
                         (seq-position magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)))))

;; https://github.com/vermiculus/magithub
(use-package magithub                   ; Magit interfaces for GitHub
  :disabled
  :after magit
  :config (magithub-feature-autoinject t))

;; https://github.com/minad/marginalia
(use-package marginalia                 ; Enrich existing commands with completion annotations
  :hook
  (after-init-hook . marginalia-mode))

;; http://jblevins.org/projects/markdown-mode/
(use-package markdown-mode              ; Major mode for Markdown-formatted text
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
  (use-package edit-indirect)   ; Edit regions in separate buffers

  (setq markdown-enable-math t)
  (setq markdown-command
        (concat
         "pandoc -f markdown+smart -t html5 -s --self-contained --css "
         (expand-file-name "data/splendor.css" user-emacs-directory)))
  (setq markdown-css-paths nil)

  (define-derived-mode rmarkdown-mode markdown-mode "Rmarkdown"
    "Mode for RMarkdown"
    (set (make-local-variable 'markdown-command-needs-filename) t)
    (set (make-local-variable 'markdown-command) (expand-file-name "rmarkdown-render" user-emacs-directory))))

(use-package midnight
  :demand
  :straight nil
  :config
  (cancel-timer midnight-timer)
  (defalias 'midnight-clean-buffer-list 'clean-buffer-list)
  (run-with-idle-timer 20 nil
                       (lambda ()
                         (run-hooks 'midnight-hook))))

;; https://github.com/tarsius/minions
(use-package minions                    ; A minor-mode menu for the mode line
  :demand
  :custom
  (minions-mode-line-lighter "[+]")
  :config
  (minions-mode 1))

(use-package message
  :straight nil
  :custom
  ;; Exit after sending message
  (message-kill-buffer-on-exit t)
  (message-send-mail-function 'smtpmail-send-it)
  (message-screenshot-command '("import" "-silent" "png:-"))
  :hook
  (message-mode-hook . turn-off-auto-fill)
  (message-mode-hook . turn-on-visual-line-mode))

;; https://gitlab.com/jessieh/mood-line
(use-package mood-line                  ; A minimal mode-line inspired by doom-modeline
  :demand :after minions
  :config
  (defun mood-line-segment-major-mode ()
    "Displays the current major mode in the mode-line."
    (concat (format-mode-line minions-mode-line-modes 'mood-line-major-mode) "  "))
  (mood-line-mode))

(use-package mu4e
  :straight (:host github :files ("build/mu4e/*.el") :repo "djcb/mu"
                   :pre-build (("./autogen.sh")
                               ("ninja" "-C" "build")))
  ;; When invoking via `state'
  :commands mu4e-running-p       ; used by state
  ;; When opening a link in an Org task
  :commands (mu4e-org-open mu4e-org-store-link)
  :init
  ;; Install link support but don't load mu4e
  (eval-after-load "org"
    '(org-link-set-parameters "mu4e"
                              :follow #'mu4e-org-open
                              :store #'mu4e-org-store-link))
  :custom
  (mu4e-mu-binary (expand-file-name "build/mu/mu" (straight--repos-dir "mu")))
  (mu4e-attachment-dir "~/deathrow")
  (mu4e-completing-read-function 'completing-read)
  (mu4e-save-multiple-attachments-without-asking t)
  (mu4e-compose-keep-self-cc nil)
  (mu4e-compose-dont-reply-to-self t)
  (mu4e-context-policy 'pick-first)
  (mu4e-compose-context-policy nil)
  (mu4e-compose-format-flowed t)
  (mu4e-use-fancy-chars t)
  (mu4e-headers-leave-behavior 'apply)

  ;; Don't save message to Sent Messages, Gmail/IMAP takes care of this
  (mu4e-sent-messages-behavior 'delete)

  ;; Make sure we use mu4e message mode to compose emails
  (mail-user-agent 'mu4e-user-agent)

  ;; "Works better" with mbsync
  (mu4e-change-filenames-when-moving t)

  (mu4e-get-mail-command "mbsync -a")

  ;; No AM date
  (mu4e-headers-time-format "%T")
  (mu4e-headers-date-format "%Y-%m-%d %H:%M")
  (mu4e-headers-fields
   '((:flags . 8)
     (:human-date . 18)
     (:mailing-list . 10)
     (:from . 22)
     (:subject)))
  (mu4e-update-interval (when (or (on-zbook) (on-knuth)) 500))
  :config
  (setq mu4e-headers-unread-mark    '("u" .  "")
        mu4e-headers-draft-mark     '("D" .  "🚧")
        mu4e-headers-flagged-mark   '("F" .  "🚩")
        mu4e-headers-new-mark       '("N" .  "")
        mu4e-headers-passed-mark    '("P" .  "↪")
        mu4e-headers-replied-mark   '("R" .  "↩")
        mu4e-headers-seen-mark      '("S" .  "")
        mu4e-headers-trashed-mark   '("T" .  "🗑️")
        mu4e-headers-attach-mark    '("a" .  "📎")
        mu4e-headers-encrypted-mark '("x" .  "🔑")
        mu4e-headers-signed-mark    '("s" .  ""))

  (add-to-list 'mu4e-bookmarks (make-mu4e-bookmark
                                :name "Unread or today"
                                :query "flag:unread OR date:today..now"
                                :key ?u))

  (add-to-list 'mu4e-bookmarks (make-mu4e-bookmark
                                :name "Unread"
                                :query "flag:unread AND NOT flag:trashed"
                                :key ?U))

  (add-to-list 'mu4e-bookmarks (make-mu4e-bookmark
                                :name "Sent"
                                :query "maildir:/utc/Sent"
                                :key ?s))

  (add-to-list 'mu4e-bookmarks (make-mu4e-bookmark
                                :name "Last month's messages"
                                :query "date:31d..now"
                                :key ?W))

  (define-key-after global-map [menu-bar tools mu4e]
    (cons "Mu4e" (make-sparse-keymap " blah")) 'tools)

  (define-key global-map [menu-bar tools mu4e dau]
    (cons "Disable auto-update"
          (lambda ()
            (interactive)
            (cancel-timer mu4e~update-timer)
            (setq mu4e-update-interval nil))))

  (defhydra hydra-mu4e-search (:foreign-keys run :base-map (make-sparse-keymap))
    "mu search keywords"
    ("C-c a" (insert "flag:attach ") "Attach")
    ("C-c p" (insert "mime:application/pdf ") "PDF")
    ("C-c s" (insert "maildir:/utc/Sent ") "Sent")
    ("C-c h" (progn (browse-url "https://www.djcbsoftware.nl/code/mu/mu4e/Queries.html")) "Help")
    ("RET" exit-minibuffer "Search" :exit t))

  (defun mu4e-headers-search-hydra ()
    (interactive)
    (minibuffer-with-setup-hook #'hydra-mu4e-search/body
      (mu4e-search
       (read-string "Search for: "))))

  (define-key mu4e-search-minor-mode-map (kbd "s") #'mu4e-headers-search-hydra)

  (require 'org-mu4e)

  ;; Use mu4e when attaching from dired
  (setq gnus-dired-mail-mode 'mu4e-user-agent)

  (require 'gnus-dired)
  ;; make the `gnus-dired-mail-buffers' function also work on
  ;; message-mode derived modes, such as mu4e-compose-mode
  (defun gnus-dired-mail-buffers ()
    "Return a list of active message buffers."
    (let (buffers)
      (save-current-buffer
        (dolist (buffer (buffer-list t))
          (set-buffer buffer)
          (when (and (derived-mode-p 'message-mode)
                     (null message-sent-message-via))
            (push (buffer-name buffer) buffers))))
      (nreverse buffers)))

  (setq gnus-dired-mail-mode 'mu4e-user-agent)
  (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

  ;; Open in emacs on M-RET
  ;; (defun mu4e~view-open-attach-from-binding ()
  ;;   "Open the attachement at point, or click location."
  ;;   (interactive)
  ;;   (let* (( msg (mu4e~view-get-property-from-event 'mu4e-msg))
  ;;          ( attnum (mu4e~view-get-property-from-event 'mu4e-attnum)))
  ;;     (when (and msg attnum)
  ;;       (mu4e-view-open-attachment-emacs msg attnum))))
  )

;; https://github.com/iqbalansari/mu4e-alert
(use-package mu4e-alert                 ; Desktop notification for mu4e
  :if (on-zbook)
  :demand :after mu4e
  :config
  (mu4e-alert-set-default-style 'libnotify)
  (mu4e-alert-enable-notifications)
  (setq mu4e-alert-interesting-mail-query
        (concat
         "flag:unread"
         " AND NOT flag:trashed"
         " AND NOT maildir:/gmail/INBOX")))

;; Using multi-term instead of term
;; http://www.emacswiki.org/emacs/download/multi-term.el
(use-package multi-term                 ; Managing multiple terminal buffers in Emacs.
  :config
  (defalias 'term 'multi-term)
  (setq multi-term-program "/bin/zsh"))

;; https://github.com/magnars/multiple-cursors.el
(use-package multiple-cursors           ; Multiple cursors for Emacs.
  :custom
  (mc/list-file "~/SynologyDrive/Sylvain/emacs/.mc-lists.el")
  :config
  (add-to-list 'mc/unsupported-minor-modes 'electric-pair-mode)
  :bind (("C-M-c" . mc/mark-next-like-this)))

;; https://github.com/ahendriksen/ob-tmux
(use-package ob-tmux                    ; Babel Support for Interactive Terminal
  :custom
  (org-babel-default-header-args:tmux
   `((:results . "silent")
     (:session . ,(format "%s@%s" (user-login-name) (system-name)))
     (:socket  . nil)
     (:terminal . "urxvt")))
  (org-babel-tmux-session-prefix "")
  :config
  (defun ob-tmux--target (ob-session)
    (let* ((target-session (ob-tmux--session ob-session))
           (window (ob-tmux--window ob-session))
           (target-window (if window (concat "=" window) "")))
      (concat target-session ":" target-window))))

(use-package octave
  :config
  (define-key octave-mode-map "\C-c\C-r" #'octave-send-region)
  (define-key octave-mode-map "\C-c\C-s" #'octave-send-buffer))

;; https://bitbucket.org/jpkotta/openwith
(use-package openwith                   ; Open files with external programs
  :preface
  (rassq-delete-all #'doc-view-mode-maybe auto-mode-alist)
  :config
  (setq openwith-associations
        '(("\\.pdf\\'" "evince" (file))
          ("\\.\\(?:mpe?g\\|avi\\|wmv\\|flv\\)\\'" "vlc" (file))
          ("\\.\\(od[ts]\\|docx?\\|xlsx?\\)\\'" "soffice" (file))))
  (add-to-list 'mm-inhibit-file-name-handlers 'openwith-file-handler)
  (openwith-mode))

;; https://github.com/oantolin/orderless
(use-package orderless                  ; Completion style for matching regexps in any order
  :demand
  :config
  (setq completion-styles '(substring orderless))
  (setq completion-category-overrides '((file (styles basic partial-completion)))))

;; Contextual capture and agenda commands for Org-mode
;; https://github.com/thisirs/org-context
(use-package org-context                ; Contextual capture and agenda commands for Org-mode
  :straight `(org-context
              :type git
              :local-repo ,(expand-file-name "org-context" projects-directory))
  :config
  (defun org-capture--add-link ()
    (format "%s %s"
            (plist-get org-store-link-plist :subject)
            (org-link-make-string
             (plist-get org-store-link-plist :link)
             "ici")))

  (let ((capture-tmpls
         '(("e" "Event")
           ("ee" "Simple event" entry
            (file+headline "~/SynologyDrive/Sylvain/Org/agenda.org" "Evénements simples")
            "\
* %?%(org-capture--add-link) %^G
  %^T
  OPENED: %U"
            :created t)
           ("es" "Scheduled event" entry
            (file+headline "~/SynologyDrive/Sylvain/Org/agenda.org" "Liste des scheduled")
            "\
* %?%(org-capture--add-link) %^G
  SCHEDULED: %^T
  OPENED: %U")
           ("ed" "Deadline event" entry
            (file+headline "~/SynologyDrive/Sylvain/Org/agenda.org" "Liste des deadlines")
            "\
* %?%(org-capture--add-link) %^G
  DEADLINE: %^T
  OPENED: %U"
            ))))
    (add-to-list 'org-context-capture-alist (cons 'mu4e-view-mode capture-tmpls))
    (add-to-list 'org-context-capture-alist (cons 'mu4e-headers-mode capture-tmpls)))

  (org-context-mode))

(use-package org-link-minor-mode
  :init
  (define-globalized-minor-mode
    global-org-link-minor-mode
    org-link-minor-mode
    (lambda () (org-link-minor-mode 1)))
  :diminish)

(use-package org-pdfview)

;; https://github.com/jkitchin/org-ref
(use-package org-ref ; citations, cross-references and bibliographies in org-mode
  :disabled
  :demand
  :config
  (setq bibtex-completion-bibliography (list (expand-file-name "recherche/biblio/refs.bib" personal-directory)))

  ;; Use bibtex-completion-find-pdf-in-field to open pdf file
  (defun bibtex-completion-find-pdf-in-field-for-org-ref (key-or-entry)
    (or (car (bibtex-completion-find-pdf-in-field key-or-entry))
        "/dummy"))
  (setq org-ref-get-pdf-filename-function 'bibtex-completion-find-pdf-in-field-for-org-ref))

(use-package org-protocol
  :straight nil
  :preface
  ;; Load org-protocol only when `server-visit-files' is called. An
  ;; advice around `server-visit-files' is present in org-protocol but
  ;; not autoloaded, so do it here.
  (autoload 'org--protocol-detect-protocol-server "org-protocol")
  (advice-add 'server-visit-files :around #'org--protocol-detect-protocol-server))

;; https://github.com/org-roam/org-roam
(use-package org-roam                   ; Roam Research replica with Org-mode
  :diminish
  :init
  (defun org-roam-dired-jump ()
    "Jump to Dired buffer of `org-roam-directory'."
    (interactive)
    (dired org-roam-directory))
  :custom
  (org-roam-directory (expand-file-name "recherche/notes" personal-directory))
  :bind (("C-c n r" . org-roam-buffer-toggle)
         ("C-c n d" . org-roam-dailies-goto-today)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n j" . org-roam-dired-jump))
  :config
  (org-roam-db-autosync-enable)

  ;; Disable auto-insert feature when capturing in new Org file
  (defun org-roam-capture--no-autoinsert (oldfun &rest args)
    "No autoinsert when capturing via `org-roam'."
    (let (auto-insert)
      (apply oldfun args)))

  (advice-add 'org-roam-capture- :around #'org-roam-capture--no-autoinsert))

;; https://github.com/org-roam/org-roam-bibtex
(use-package org-roam-bibtex            ; Org Roam meets BibTeX
  :diminish
  :commands org-roam-bibtex-open-citekey
  :preface
  ;; Add support for org-protocol links of the form:
  ;; org-protocol://roam-citekey?template=r&citekey=belkin_reconciling_2018.
  ;; The function `org-roam-bibtex-open-citekey' handles the link by
  ;; editing the note corresponding to citekey.
  (with-eval-after-load 'org-protocol
      (push '("org-roam-citekey" :protocol "roam-citekey" :function org-roam-bibtex-open-citekey)
            org-protocol-protocol-alist))
  :custom
  ;; Use Org syntax to cite: [cite:@citekey]
  (orb-roam-ref-format 'org-cite)
  :config
  (defun org-roam-capture-add-tags ()
    (mapconcat
     'identity
     (completing-read-multiple
      "Tag(s): " (org-roam-tag-completions))
     " "))

  ;; Immediately file capture and display file: add :immediate-finish
  ;; and :jump-to-captured.
  (add-to-list 'org-roam-capture-templates
               '("r" "bibliography reference" plain "%?"
                 :target
                 (file+head "${citekey}.org" "#+title: ${title}\n#+authors: ${author}\n#+year: ${date}\n#+filetags: %(org-roam-capture-add-tags)")
                 :unnarrowed t
                 :jump-to-captured t
                 :immediate-finish t))

  (defun org-roam-bibtex-open-citekey (info)
    "Open an Org-roam note from an Org protocol call."
    (unless (plist-get info :citekey)
      (user-error "No citekey key provided"))
    (unless (plist-get info :template)
      (user-error "No template key provided"))
    (org-roam-plist-map! (lambda (k v)
                           (org-link-decode
                            (if (equal k :ref)
                                (org-protocol-sanitize-uri v)
                              v))) info)
    (raise-frame)
    (let ((org-roam-capture-templates (list (assoc (plist-get info :template) org-roam-capture-templates))))
      (orb-edit-note (plist-get info :citekey)))))

(use-package org-roam-protocol
  :straight nil
  :demand :after org-protocol)

;; https://github.com/org-roam/org-roam-ui
(use-package org-roam-ui                ; User Interface for Org-roam
  :after org-roam
  :diminish)

;; https://github.com/integral-dw/org-superstar-mode
(use-package org-superstar              ; Prettify headings and plain lists in Org mode
  :demand :after org
  :custom
  (org-superstar-headline-bullets-list '("•"))
  (org-hide-leading-stars t)
  (org-ellipsis "⤵")
  :hook
  (org-mode-hook . org-superstar-mode))


;; http://elpa.gnu.org/packages/orgalist.html
(use-package orgalist                   ; Manage Org-like lists in non-Org buffers
  :demand :after mu4e
  :config
  (add-to-list 'orgalist-context-function
               '(mu4e-compose-mode . orgalist-message-mode-context))
  (add-hook 'mu4e-compose-mode-hook #'orgalist-mode))

(use-package ox-ipynb
  :straight (ox-ipynb :type git :host github :repo "jkitchin/ox-ipynb"))

;; https://github.com/Malabarba/paradox
(use-package paradox                    ; A modern Packages Menu. Colored, with package ratings, and customizable.
  :disabled
  :commands paradox-list-packages
  :config
  (setq paradox-github-token t)
  (setq paradox-execute-asynchronously t))

(use-package paredit
  :preface
  (defvar paredit-minibuffer-commands '(ibuffer-do-eval
                                        ibuffer-do-view-and-eval
                                        edebug-eval-expression
                                        edebug-set-conditional-breakpoint)
    "Interactive commands for which paredit should be enabled in the minibuffer.")

  (defun conditionally-enable-paredit-mode ()
    "Enable paredit during lisp-related minibuffer commands."
    (if (memq this-command paredit-minibuffer-commands)
        (enable-paredit-mode)))

  :hook
  (minibuffer-setup-hook . conditionally-enable-paredit-mode)
  ((eval-expression-minibuffer-setup-hook emacs-lisp-mode-hook lisp-mode-hook lisp-interaction-mode-hook ielm-mode-hook) . paredit-mode))

(use-package paren
  :straight nil
  :demand
  :custom
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  :config
  (show-paren-mode))

(use-package pcache)            ; persistent caching for Emacs.

;; http://github.com/vedang/pdf-tools/
(use-package pdf-tools                  ; Support library for PDF documents.
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
  (pdf-loader-install :no-query)

  (add-hook 'pdf-isearch-minor-mode-hook (lambda () (ctrlf-local-mode -1)))

  (use-package pdf-sync
    :straight nil
    :bind (:map
           pdf-sync-minor-mode-map
           ("C-c C-v" . (lambda () (interactive) (pdf-sync-backward-search 0 0)))))

  (define-key pdf-view-mode-map (kbd "M-w") 'pdf-view-kill-ring-save)

  (setq pdf-misc-print-programm lpr-command)

  (use-package pdf-annot
    :straight nil
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
           ("C-c C-d" . pdf-annot-delete-current))))

;; https://github.com/thisirs/pdf-tools-points.git
(use-package pdf-tools-points          ; Offline annotation with pdf-tools and tikz
  :straight `(pdf-tools-points :local-repo ,(expand-file-name "pdf-tools-points" projects-directory))
  :after pdf-tools :demand)

;; https://github.com/ejmr/php-mode
(use-package php-mode)          ; Major mode for editing PHP code

;; https://github.com/vitoshka/polymode
(use-package polymode                   ; Versatile multiple modes with extensive literate programming support
  :commands poly-latex-mode
  :config
  (defcustom  pm-inner/latex-code-environment
    (pm-inner-chunkmode :name "latex-code-environment"
                        :head-matcher "^[ \t]*\\\\begin{knitr}\\(?:\\[\\([^]]+\\)\\]\\)?\n"
                        :tail-matcher "^[ \t]*\\\\end{knitr}"
                        :mode 'python-mode
                        :head-mode 'host
                        :tail-mode 'host
                        )
    "Latex code environment."
    :group 'poly-innermodes
    :type 'object)

  (define-polymode poly-latex-mode
    :hostmode 'pm-host/latex
    :innermodes '(pm-inner/latex-code-environment)))

;; https://github.com/raxod502/prescient.el
(use-package prescient                  ; Better sorting and filtering
  :demand
  :config
  (prescient-persist-mode)
  :custom
  ((prescient-save-file (expand-file-name "prescient-save.el" personal-emacs-directory))
   (prescient-sort-length-enable nil)
   (prescient-aggressive-file-save t)
   (selectrum-fix-vertical-window-height t)))

;; https://elpa.gnu.org/packages/project.html
(use-package project                    ; Operations on the current project
  :straight nil
  :custom (project-switch-commands 'project-dired)
  :config
  (defun project-reload-projects ()
    (interactive)
    (dolist (dir '("~/SynologyDrive/Sylvain/enseignements/repositories/"
                   "~/.emacs.d/straight/repos/"
                   "~/repositories"
                   "~/SynologyDrive/Sylvain/projects"))
      (when (file-directory-p dir)
        (project-remember-projects-under dir))))

  (defun project-try-known (dir)
    (when (member dir (directory-files "~/SynologyDrive/Sylvain/enseignements/repositories/" 'full "^[^.]"))
      (cons 'known dir)))

  (cl-defmethod project-root ((project (head known)))
    "Return root directory of known PROJECT."
    (cdr project))

  (add-to-list 'project-find-functions 'project-try-known))

;; Projectile
;; https://github.com/bbatsov/projectile
(use-package projectile                 ; Manage and navigate projects in Emacs easily
  :disabled
  :init
  ;; Auto-remove non-existent projects
  (run-with-idle-timer 10 nil #'projectile-cleanup-known-projects)

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
  (setq projectile-switch-project-action #'projectile-dired)

  (setq projectile-mode-line-function #'projectile-custom-mode-line)

  (setq projectile-completion-system 'default)

  ;; Use custom function to add specific projects
  (add-to-list 'projectile-project-root-functions 'projectile-fake-projects)

  ;; Add list of default projects
  (mapc #'projectile-add-known-project (projectile-default-projects))

  (setq projectile-require-project-root nil)

  ;; Use custom function to ignore specific projects
  (setq projectile-ignored-project-function #'projectile-ignored-semester)

  ;; Unconditionaly add these projects
  (setq projectile-project-search-path (list (expand-file-name "projects" personal-directory)))

  (projectile-mode))

;; https://github.com/fgallina/python.el
(use-package python                     ; Python's flying circus support for Emacs
  :straight nil
  :config
  (setq python-indent-guess-indent-offset-verbose nil
        python-shell-interpreter-args "-m IPython -i --simple-prompt")

  (defvar python-indent-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map "<" 'python-indent-shift-left)
      (define-key map ">" 'python-indent-shift-right)
      map))

  (put 'python-indent-shift-left 'repeat-map 'python-indent-repeat-map)
  (put 'python-indent-shift-right 'repeat-map 'python-indent-repeat-map))

;; http://elpa.gnu.org/packages/rainbow-mode.html
(use-package rainbow-mode)      ; Colorize color names in buffers

;; https://github.com/ChillarAnand/real-auto-save
(use-package real-auto-save             ; Automatically save your buffers/files at regular intervals
  :disabled
  :demand
  :config
  (real-auto-save-mode +1)
  (setq real-auto-save-buffers-list nil)
  (with-eval-after-load 'org
    (setq real-auto-save-buffers-list org-agenda-files)))

;; https://github.com/purcell/reformatter.el
(use-package reformatter               ; Define commands which run re-formatters
  :preface
  ;; Don't config reformatter but define reformatter functions to
  ;; trigger their own redefinition in :config.
  (autoload-config reformatter-black-region reformatter)
  (autoload-config reformatter-black-buffer reformatter)
  :config
  (require 'exec-path-from-shell) ; for ~/.local/bin
  (when (zerop (call-process-shell-command "Rscript -e \"quit(status = ifelse(require(formatR), 0, 1))\""))
    (reformatter-define reformatter-R
      :program "Rscript"
      :args (list "-e" "library(formatR); tidy_source(file('stdin', 'r'), arrow = TRUE, width.cutoff = 500)")))

  (when (executable-find "latexindent")
    (reformatter-define reformatter-latex
      :program "latexindent"
      :args (list "-y=defaultIndent:'  '")))

  (cond ((executable-find "black-macchiato-strip")
         (reformatter-define reformatter-black
           :program "black-macchiato-strip"
           :args (list "--target-version" "py310")))
        ((executable-find "black-macchiato")
         (reformatter-define reformatter-black
           :program "black-macchiato"
           :args (list "--target-version" "py310"))))

  (when (executable-find "npx")
    (reformatter-define reformatter-sql
      :program "npx"
      :args (list "sql-formatter" "-u")))

  (when (executable-find "sqlformat")
    (reformatter-define reformatter-sql
      :program "sqlformat"
      :args (list "-k" "upper" "-r" "-")))

  (when (executable-find "sql-formatter-cli")
    (reformatter-define reformatter-sql
      :program "sql-formatter-cli"
      :args (list "-")))

  (when (executable-find "/snap/bin/shfmt")
    (reformatter-define reformatter-bash
      :program "/snap/bin/shfmt"
      :lighter " ShFmt"))

  (reformatter-define reformatter-styler
    :program "Rscript"
    :args (list "--vanilla" "-e" "con <- file(\"stdin\")
out <- styler::style_text(readLines(con))
close(con)
out")
    :lighter " styler"))


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

(use-package repeat
  :straight nil
  :hook (after-init-hook . repeat-mode))

;; A search tool based on ripgrep
;; https://github.com/dajva/rg.el
(use-package rg                         ; A search tool based on ripgrep
  :disabled
  :if (executable-find "rg")
  :bind ("M-g f" . rg-custom-search)
  :config
  (add-hook 'rg-mode-hook (lambda () (interactive) (toggle-truncate-lines t)))
  (rg-define-search rg-custom-search
    :format regexp
    :dir current
    :files "*"))

;; https://github.com/dgutov/robe
(use-package robe                       ; Code navigation, documentation lookup and completion for Ruby
  :hook (ruby-mode-hook . robe-mode))

;; https://github.com/raxod502/prescient.el
(use-package selectrum-prescient        ; Selectrum integration
  :disabled
  :demand
  :bind (:map selectrum-minibuffer-map
              ("C-M-j" . selectrum-submit-exact-input)
              ("C-j" . selectrum-insert-current-candidate))
  :config
  (setq selectrum-count-style 'current/matches)
  (selectrum-mode +1)
  (setq selectrum-highlight-candidates-function #'orderless-highlight-matches)
  (setq selectrum-refine-candidates-function #'orderless-filter)
  (setq selectrum-prescient-enable-filtering nil)
  (selectrum-prescient-mode +1)
  (prescient-persist-mode)

  (when (require 'embark nil t)
    (define-key selectrum-minibuffer-map (kbd "C-c C-o") 'embark-export)
    (define-key selectrum-minibuffer-map (kbd "C-c C-c") 'embark-act))

  :custom
  ((prescient-save-file (expand-file-name "prescient-save.el" personal-emacs-directory))
   (prescient-sort-length-enable nil)
   (prescient-aggressive-file-save t)
   (selectrum-fix-vertical-window-height t)))

;; https://github.com/twlz0ne/separedit.el
(use-package separedit                  ; Edit comment/string/docstring/code block in separate buffer
  :bind (:map prog-mode-map
              ("C-c '" . separedit))
  :custom
  (separedit-remove-trailing-spaces-in-comment t)
  (separedit-default-mode 'markdown-mode))

(use-package simple
  :straight nil
  :custom
  (line-move-visual t)
  :preface
  ;; From https://with-emacs.com/posts/tips/quit-current-context/
  (defun keyboard-quit-context+ ()
    "Quit current context.

This function is a combination of `keyboard-quit' and
`keyboard-escape-quit' with some parts omitted and some custom
behavior added."
    (interactive)
    (cond ((region-active-p)
           ;; Avoid adding the region to the window selection.
           (setq saved-region-selection nil)
           (let (select-active-regions)
             (deactivate-mark)))
          ((eq last-command 'mode-exited) nil)
          (current-prefix-arg
           nil)
          (defining-kbd-macro
            (message
             (substitute-command-keys
              "Quit is ignored during macro defintion, use \\[kmacro-end-macro] if you want to stop macro definition"))
            (cancel-kbd-macro-events))
          ((active-minibuffer-window)
           (when (get-buffer-window "*Completions*")
             ;; hide completions first so point stays in active window when
             ;; outside the minibuffer
             (minibuffer-hide-completions))
           (abort-recursive-edit))
          (t
           (when completion-in-region-mode
             (completion-in-region-mode -1))
           (let ((debug-on-quit nil))
             (signal 'quit nil)))))

  :bind ([remap keyboard-quit] . #'keyboard-quit-context+))

;; Smart modeline
;; http://github.com/Malabarba/smart-mode-line
(use-package smart-mode-line            ; A color coded smart mode-line.
  :disabled
  :if (window-system)
  :commands sml/setup
  :demand
  :init
  (setq sml/theme 'respectful
        sml/shorten-directory t
        sml/shorten-modes t
        sml/name-width 40
        sml/mode-width 'full)
  :config
  (sml/setup))

;; https://github.com/Fuco1/smartparens
(use-package smartparens                ; Automatic insertion, wrapping and paredit-like navigation with user defined pairs.
  :disabled
  :config
  (sp-local-pair
   '(markdown-mode gfm-mode)
   "\`\`\`" "\`\`\`" :post-handlers '(("||\n" "RET"))))

;; http://github.com/nonsequitur/smex/
(use-package smex                       ; M-x interface with Ido-style fuzzy matching.
  :disabled
  :defer 10
  :config
  (smex-initialize))

(use-package saveplace
  :hook (after-init-hook . save-place-mode))

(use-package skeletor)          ; Provides project skeletons for Emacs

;; https://github.com/yuya373/emacs-slack
(use-package slack                      ; Slack client for Emacs
  :disabled
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
  :demand
  :config
  (smart-mark-mode 1))

;; Minor mode to resolve diff3 conflicts
(use-package smerge-mode
  :hook (find-file-hook . sm-try-smerge)
  :config
  (defun sm-try-smerge ()
    "Turn on smerge-mode if there is a diff marker."
    (let ((old-point (point)))
      (goto-char (point-min))
      (if (re-search-forward "^\\(<\\)\\{7\\} " nil t)
          (smerge-mode 1)
        (goto-char old-point)))))

;; https://savannah.nongnu.org/projects/so-long
(use-package so-long                    ; Say farewell to performance problems with minified code.
  :straight nil
  :hook (after-init-hook . global-so-long-mode))

(use-package spotify.el
  :disabled
  :straight (spotify.el :type git :host github :repo "danielfm/spotify.el"
                     :files ("*.el")))

;; https://github.com/thisirs/state.git
(use-package state                      ; Quick navigation between workspaces
  :diminish
  :preface
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

  ;; Override state's keymap binding
  :bind-keymap ("s-s" . state-prefix-map)
  :config
  (state-define-state org-roam
    :key "b"
    :in (and (featurep 'org-roam)
             (or (eq (buffer-name) org-roam-buffer)
                 (state--in-in-file org-roam-directory)))
    :create
    (progn
      (dired org-roam-directory)
      (delete-other-windows)
      (org-roam)))

  (state-define-state agenda
    :key "a"
    :in (eq major-mode 'org-agenda-mode)
    :switch (org-agenda nil "a"))

  (state-define-state debug
    :key "d"
    :switch "*debug*")

  (state-define-state mu4e
    :key "u"
    :in (memq major-mode '(mu4e-main-mode mu4e-headers-mode mu4e-view-mode mu4e-compose-mode))
    :exist (mu4e-running-p)
    :create (progn
              (mu4e)
              (delete-other-windows)))

  (state-define-state message
    :key "m"
    :switch "*Messages*")

  (state-define-state scratch
    :key "s"
    :switch "*scratch*")

  (state-define-state programming_samples
    :key "r"
    :switch "~/SynologyDrive/Sylvain/Org/programming_samples.org")

  (state-define-state personnal
    :key "p"
    :switch "~/SynologyDrive/Sylvain/Org/personnel.org.gpg"
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

  (state-define-state erc
    :key "i"
    :in (and (fboundp 'erc-buffer-list)
             (memq (current-buffer) (erc-buffer-list)))
    :switch (progn (erc-start-or-switch 1)
                   (delete-other-windows)))

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

  (state-define-repl elisp-repl "j" "*ielm*" (memq major-mode '(lisp-interaction-mode emacs-lisp-mode)) (ielm))
  (state-define-repl matlab-repl "j" "*MATLAB*" (eq major-mode 'matlab-mode) (matlab-shell))
  (state-define-repl python-repl "j" "*Python*" (eq major-mode 'python-mode) (call-interactively 'run-python))
  (state-define-repl ruby-repl "j" "*ruby*" (eq major-mode 'ruby-mode) (inf-ruby))
  (state-define-repl R-repl "j" "*R*" (eq major-mode 'ess-mode) (let (ess-ask-for-ess-directory) (R)))

  (state-define-state elfeed
    :key "l"
    :switch elfeed
    :in (memq major-mode '(elfeed-show-mode elfeed-search-mode)))

  (state-global-mode 1))

;; https://github.com/nflath/sudo-edit
(use-package sudo-edit                  ; Open files as another user
  :config
  (sudo-edit-indicator-mode))

;; https://github.com/abo-abo/swiper
(use-package swiper                     ; Isearch with an overview. Oh, man!
  :disabled
  :bind (:map ivy-minibuffer-map
              ("C-w" . ivy-yank-word))
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

;; http://www.emacswiki.org/elisp/tidy.el
(use-package tidy)              ; Interface to the HTML Tidy program

;; https://git.sr.ht/~protesilaos/tmr
(use-package tmr)                       ; Set timers using a convenient notation

;; Taken from https://github.com/raxod502/radian/blob/develop/emacs/radian.el
;; https://github.com/magit/transient
(use-package transient                  ; Transient commands
  :config
  ;; Allow using `q' to quit out of popups, in addition to `C-g'. See
  ;; <https://magit.vc/manual/transient.html#Why-does-q-not-quit-popups-anymore_003f>
  ;; for discussion.
  (transient-bind-q-to-quit))

(use-package transpose-frame            ; Transpose windows arrangement in a frame
  :bind ("<C-kp-multiply>" . rotate-frame-anticlockwise))

;; Tramp env to properly display dired
;; https://www.gnu.org/software/tramp/
(use-package tramp                      ; Transparent Remote Access, Multiple Protocol
  :straight nil
  :custom
  (remote-file-name-inhibit-locks t)
  :config
  ;; (add-to-list 'tramp-remote-process-environment "LC_ALL=en_US.utf8" 'append)
  )

;; http://www.dr-qubit.org/emacs.php
(use-package undo-tree                  ; Treat undo history as a tree
  :demand
  :bind (:map undo-tree-visualizer-mode-map ("RET" . undo-tree-visualizer-quit))
  :diminish
  :custom
  (undo-tree-auto-save-history nil)
  (undo-tree-visualizer-timestamps t)
  (undo-tree-enable-undo-in-region t)
  (undo-tree-visualizer-diff t)
  :config
  (global-undo-tree-mode))

;; https://github.com/purcell/unfill
(use-package unfill                     ; Unfill paragraphs or regions, and toggle between filled & unfilled
  :bind ([remap fill-paragraph] . unfill-toggle))

;; https://github.com/thisirs/vc-check-status
(use-package vc-check-status            ; Warn you when quitting emacs and leaving repo dirty.
  :defer 5
  :config
  ;; Be sure to leave my packages' repo on master
  (push '("~/SynologyDrive/Sylvain/emacs/site-lisp/" (not-on-branch "master")) vc-check-alist)

  ;; Only look for unpushed commits on master
  (push '("~/.emacs.d" (unpushed "master") changes) vc-check-alist)

  ;; Don't check on auto-committed repo
  (add-to-list 'vc-check-cancel-hook
               (lambda ()
                 (and
                  (fboundp 'vc-auto-commit-backend)
                  (vc-auto-commit-backend))))

  (vc-check-status-activate))

;; https://github.com/minad/vertico
(use-package vertico                    ; VERTical Interactive COmpletion
  :hook (after-init-hook . vertico-mode))

;; https://github.com/benma/visual-regexp.el/
(use-package visual-regexp              ; A regexp/replace command for Emacs with interactive visual feedback
  :commands (vr/query-replace vr/replace)
  :bind* (("C-c r" . vr/replace)
          ("C-c q" . vr/query-replace)
          ("M-%" . vr/query-replace)))

;; https://github.com/benma/visual-regexp-steroids.el/
(use-package Visual-regexp-steroids   ; Extends visual-regexp to support other regexp engines
  :commands (vr/select-replace vr/select-query-replace)
  :demand :after visual-regexp)

;; http://github.com/thisirs/vc-auto-commit.git
(use-package vc-auto-commit             ; Auto-committing feature for your repository
  :straight `(vc-auto-commit :local-repo ,(expand-file-name "vc-auto-commit" projects-directory))
  :defer 5
  :commands (vc-auto-commit-backend)
  :bind ("C-x v C" . vc-auto-commit)
  :config
  (defun not-on-zbook (root backend)
    (not (on-zbook)))
  (add-hook 'vc-auto-commit-cancel-hook #'not-on-zbook)
  (vc-auto-commit-activate))

(use-package warnings
  :straight nil
  :config
  (add-to-list 'warning-suppress-types '(comp)))

(use-package webjump
  :disabled
  :bind ("C-c j" . webjump))

;; http://github.com/mhayashi1120/Emacs-wgrep/raw/master/wgrep.el
(use-package wgrep)                     ; Writable grep buffer and apply the changes to files

;; https://github.com/justbur/emacs-which-key
(use-package which-key                  ; Display available keybindings in popup
  :disabled
  :demand
  :diminish
  :config
  ;; Trigger which-key on C-h or on specific keystrokes
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay 10000)
  (setq which-key-idle-secondary-delay 0.05)
  (setq which-key-allow-regexps
        '("C-c n" "C-x RET"))
  (which-key-mode))

(use-package whitespace
  :demand
  :straight nil
  :config
  (setq whitespace-style
        '(face trailing tabs))
  (global-whitespace-mode))

(use-package windmove
  :straight nil
  :bind
  ("s-b" . windmove-left)
  ("s-f" . windmove-right)
  ("s-p" . windmove-up)
  ("s-n" . windmove-down))

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
(use-package wtf
  :disabled
  :commands wtf-is)

;; Buffers can't have the same name
(with-eval-after-load 'uniquify
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  (setq uniquify-after-kill-buffer-p t))

;; https://github.com/yoshiki/yaml-mode
(use-package yaml-mode)         ; Major mode for editing YAML files

;; https://github.com/AndreaCrotti/yasnippet-snippets
(use-package yasnippet-snippets         ; Collection of yasnippet snippets
  :demand :after yasnippet
  :straight (yasnippet-snippets
             :type git
             :host github
             :repo "AndreaCrotti/yasnippet-snippets"
             :files ("*.el" ("snippets" "snippets/python-mode"))))

;; http://www.emacswiki.org/zoom-frm.el
(use-package zoom-frm                   ; Commands to zoom frame font size.
  :bind (("C-<down-mouse-4>" . zoom-in)
         ("C-<down-mouse-5>" . zoom-out)
         ("C-c +" . zoom-in)
         ("C-c -" . zoom-out)
         ("<C-kp-add>" . zoom-in)
         ("<C-kp-subtract>" . zoom-out)))

;; https://github.com/thierryvolpiatto/zop-to-char
(use-package zop-to-char                ; A replacement of zap-to-char.
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
                 (shell-command (format "tmux send C-u C-k \"setwd(\\\"%s\\\")\" ENTER"
                                        (file-truename default-directory)))))
              ((string-match "python[23]?" current-command)
               (let ((coding-system-for-write 'utf-8))
                 (shell-command (format "tmux send C-u C-k \"import os; os.chdir(\\\"%s\\\")\" ENTER"
                                        (file-truename default-directory)))))
              (t (let ((coding-system-for-write 'utf-8))
                   (shell-command (format "tmux send C-u \"cd \\\"%s\\\"\" ENTER"
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
(require 'notifications)

;; Find pdf at a ref which has the same name in `pdfs-directory'
(defvar pdfs-directory nil
  "Directory to look for pdf files.")

(put 'pdfs-directory 'safe-local-variable 'string-or-null-p)


(use-package ffap
  :config
  (defun ffap-bib-latex-mode (bib_id)
    "Infer a filename from BIB_ID."
    (when (eq major-mode 'latex-mode)
      (if (and pdfs-directory (file-exists-p (concat pdfs-directory bib_id ".pdf")))
          (concat pdfs-directory name ".pdf")
        (condition-case nil
            (concat (file-name-directory (car (reftex-get-bibfile-list))) bib_id ".pdf")
          (error nil)))))
  ;; Find file at point even if it is the wrong user: /home/otheruser/.bashrc
  (add-to-list 'ffap-alist
               (cons "\\`\\(/home/[^/]+\\)"
                     (lambda (name)
                       (replace-match "~" nil nil name 1))))
  (add-to-list 'ffap-alist '(latex-mode . ffap-bib-latex-mode))
  (add-to-list 'ffap-alist '(org-mode . ffap-bib-latex-mode))
  (setq ffap-file-name-with-spaces t))


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

;; Using modified version of autoinsert to allow multiple autoinsert
;; https://github.com/thisirs/auto-insert-multiple.git
(use-package autoinsert
  :straight `(auto-insert-multiple
             :type git
             :local-repo ,(expand-file-name "auto-insert-multiple"
                                            projects-directory))
  :hook (find-file-hook . auto-insert)
  :config
  ;; Reset templates
  (setq auto-insert-alist nil)

  ;; Load templates from yasnippet
  (require 'yasnippet)

  (setq auto-insert 'other)
  (setq auto-insert-query 'multiple)

  (setq auto-insert-directory (expand-file-name "~/.emacs.d/autoinsert/")))

(cond ((member "Cascadia Code" (font-family-list))
       (set-frame-font "Cascadia Code-14" nil t))
      ((member "JetBrains Mono" (font-family-list))
       (set-frame-font "JetBrains Mono-13" nil t))
      ((member "Fira Mono" (font-family-list))
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

(use-package diff
  :config
  ;; Unified diff format and no whitespace when using `diff'
  (setq diff-switches "-u -w"))

;; No limit on how many lines to keep in *Messages* buffer
(setq message-log-max t)

;; No fringe on the right
(set-fringe-mode '(8 . 0))

;; No fringe in minibuffer
(set-window-fringes (minibuffer-window) 8 0)

;; Don't automatically split vertically
(on-zbook
 (setq split-height-threshold nil))

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

(setq lexical-binding t)

;;; Taken from https://with-emacs.com/posts/projectize-commands-using-numeric-arguments/
(defun projectize-1 (cmd f &rest args)
  (if (eq cmd real-this-command)
      (let* ((proot
              (and (= 0 (prefix-numeric-value
                         current-prefix-arg))
                   (vc-responsible-backend-root default-directory)))
             (current-prefix-arg
              (and (not (= 0 (prefix-numeric-value
                              current-prefix-arg)))
                   current-prefix-arg))
             (default-directory
               (or proot default-directory))
             (buffer-file-name
              (or proot buffer-file-name)))
        ;; Update: simplified interactive call
        ;; thanks to /u/SlowValue
        (call-interactively f))
    (apply f args)))

(defun projectize (&rest cmds)
  (dolist (cmd cmds)
    (advice-add cmd :around
                (lambda (f &rest args)
                  (interactive)
                  (apply #'projectize-1 cmd f args)))))

;; (projectize #'rg-custom-search)

;; No fast scrolling
(setq mouse-wheel-progressive-speed nil)

;; Bar cursor
(setq-default cursor-type 'box)

;; Don't blink the cursor
(blink-cursor-mode -1)

;; Drive out the mouse when it's too near to the cursor.
(mouse-avoidance-mode 'jump)

;;; Prevent Extraneous Tabs
(setq-default indent-tabs-mode nil)

;; Follow links to version-controlled files
(setq vc-follow-symlinks t)

;; Use "y" and "n" in yes-or-no-p or read-answer
(setq use-short-answers t)

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
      (setq arg (if (> arg 0) (1- arg) (1+ arg))))))

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
(setq echo-keystrokes 1e-6)

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
  '("~/Nextcloud/Shared/")
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
(use-package savehist
  :hook (after-init-hook . savehist-mode)
  :config
  (add-to-list 'savehist-additional-variables 'search-ring)
  (add-to-list 'savehist-additional-variables 'regexp-search-ring))

;; Always add a final newline
(setq require-final-newline t)

;; Display only time in the modeline
(setq display-time-format "%k:%M %b %y")
(setq display-time-string-forms
      '((propertize
         (format-time-string display-time-format now)
         'help-echo
         (format-time-string "%a %b %e, %Y" now))))

;; Display time in the modeline
(display-time-mode t)

(setq sentence-end-double-space nil)

;; Save existing clipboard text into kill ring before replacing it.
(setq save-interprogram-paste-before-kill t)

;; Don't ask when killing processes on exit.
(setq confirm-kill-processes nil)

;; Don't ask before killing a buffer that has a running process.
(delq 'process-kill-buffer-query-function
      kill-buffer-query-functions)

;; Make URLs/mail adresses in comments/strings highlighted and clickable
(global-goto-address-mode +1)

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

;; Make files with a shebang executable
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Open bash-fc-* files from fc command or C-x C-e in terminal in sh-mode
(add-to-list 'auto-mode-alist '("bash-fc-[0-9]+\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("PKGBUILD\\'" . sh-mode))

;; Taken from http://emacs.stackexchange.com/questions/519/key-bindings-specific-to-a-buffer
(defvar temp-mode-map (make-sparse-keymap)
  "Keymap while temp-mode is active.")

(define-minor-mode temp-mode
  "A temporary minor mode to be activated only specific to a buffer."
  :lighter " Temp"
  :map temp-mode-map)

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

;;; init.el ends here

[
 ;; http://github.com/nflath/hungry-delete
 (use-package hungry-delete             ; hungry delete minor mode
   :config
   (setq hungry-delete-chars-to-skip " \t\r\f\v")

   (defun modi/turn-off-hungry-delete-mode ()
     "Turn off hungry delete mode."
     (hungry-delete-mode -1))

   ;; Enable `hungry-delete-mode' everywhere ..
   (global-hungry-delete-mode)

   (setq hungry-delete-join-reluctantly t)

   ;; Except ..
   ;; `hungry-delete-mode'-loaded backspace does not work in `wdired-mode',
   ;; i.e. when editing file names in the *Dired* buffer.
   (add-hook 'wdired-mode-hook #'modi/turn-off-hungry-delete-mode))
 ]
