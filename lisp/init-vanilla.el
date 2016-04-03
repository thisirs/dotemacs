;; Vanilla: load vanilla-emacs + features for testing purposes.


(defvar vanilla-features nil
  "List of vanilla features available.")

(defmacro new-vanilla (feat &rest body)
  "Macro that defines a vanilla-feat macro that expands to BODY."
  (declare (indent 1) (debug t))
  (add-to-list 'vanilla-features feat)
  `(defmacro ,(intern (concat "vanilla-" (symbol-name feat))) ()
     '(progn ,@body)))

(defun vanilla-emacs (arg &rest features)
  "Run a vanilla emacs with loaded features FEATURES. If ARG is
non-nil, the features are not loaded but just inserted in the
scratch buffer. The settings of a feature are described by the
macro `new-vanilla'."
  (interactive
   (cons current-prefix-arg
         (mapcar 'intern (split-string (read-string "Features: ")))))
  (let* ((settings `(progn ,@(mapcar
                              (lambda (feature)
                                (let ((vfeat (intern (format "vanilla-%s" feature))))
                                  (if (fboundp vfeat)
                                      (macroexpand `(,vfeat))
                                    (macroexpand `(,feature)))))
                              features)))
         (form (if arg `(with-current-buffer "*scratch*"
                          (insert (pp-to-string ',settings)))
                 `,settings))
         (program-args (list "-Q" "--eval" (prin1-to-string form)))
         (proc (let ((process-connection-type nil))
                 (apply #'start-process "emacs" nil "emacs" program-args))))))

(new-vanilla org
  (add-to-list 'load-path
               (expand-file-name "~/.emacs.d/site-lisp/org-mode/lisp"))
  (require 'org))


(new-vanilla helm
  (setq default-frame-alist '((vertical-scroll-bars . nil)
                              (tool-bar-lines . 0)
                              (menu-bar-lines . 0)
                              (fullscreen . nil)))
  (blink-cursor-mode -1)
  (add-to-list 'load-path
               (expand-file-name "~/.emacs.d/site-lisp/emacs-helm"))
  (require 'helm-config)
  (helm-mode 1)
  (define-key global-map [remap find-file] #'helm-find-files)
  (define-key global-map [remap occur] #'helm-occur)
  (define-key global-map [remap list-buffers] #'helm-buffers-list)
  (define-key lisp-interaction-mode-map
    [remap indent-for-tab-command] #'helm-lisp-completion-at-point-or-indent)
  (define-key emacs-lisp-mode-map
    [remap indent-for-tab-command] #'helm-lisp-completion-at-point-or-indent))

(new-vanilla expand-region
  (add-to-list 'load-path
               (expand-file-name "~/.emacs.d/site-lisp/expand-region.el"))
  (require 'expand-region)
  (global-set-key (kbd "C-=") 'er/expand-region)
  (setq expand-region-fast-keys-enabled nil)
  (setq er--show-expansion-message t))

(new-vanilla auctex
  (require 'package)
  (setq package-user-dir "/tmp/elpa")
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t)

  (package-initialize)

  (setq my-packages '(auctex))

  (when (memq nil (mapcar 'package-installed-p my-packages))
    (message "Refreshing packages database...")
    (package-refresh-contents)
    (dolist (p my-packages)
      (when (not (package-installed-p p))
        (package-install p))))

  ;;(load "preview.el" nil t t)
  (setq auto-mode-alist (cons '("\\.tex$" . LaTeX-mode) auto-mode-alist)))

(new-vanilla magit
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/magit/"))
  (require 'magit)
  (global-set-key "\C-ci" 'magit-status))

(new-vanilla state
  (add-to-list 'load-path "~/CloudStation/Sylvain/emacs/site-lisp/state/")
  (require 'state)
  (load "/home/sylvain/repositories/dotemacs/init-state.el"))

(new-vanilla caldav
  (progn
    (add-to-list 'load-path "/home/sylvain/.emacs.d/site-lisp")
    (add-to-list 'load-path "/home/sylvain/.emacs.d/elpa/org-caldav-20150131.152/")


    (require 'org)
    (require 'org-caldav)

    (setq org-id-locations-file "~/CloudStation/Sylvain/Org/.org-id-locations")
    (setq org-caldav-url "https://localhost:5006/web")

    (setq network-security-level 'low)
    (setq auth-sources '((:source (:secrets "emacs"))))

    (setq org-caldav-select-tags '("caldav"))
    (setq org-caldav-calendar-id "calendar-test")

    (setq org-caldav-inbox '(file+headline "~/CloudStation/Sylvain/Org/agenda.org" "External events"))
    (setq org-caldav-files '("~/CloudStation/Sylvain/Org/agenda.org"))

    (setq org-icalendar-timezone "Europe/Paris")
    (setq org-icalendar-alarm-time 15)

    ;; Export scheduled item
    (setq org-icalendar-use-scheduled '(event-if-not-todo todo-start))

    (setq org-caldav-sync-changes-to-org 'all)
    (setq org-caldav-debug-level 2)

    ;; Place to save state file
    (setq org-caldav-save-directory "~/CloudStation/Sylvain/Org/")

    ;; Fix org-icalendar error
    (setq org-ascii-paragraph-spacing 0)

    (setq org-caldav-debug-level 2)

    ;; (org-caldav-sync)
    ;; (princ (with-current-buffer "*org-caldav-debug*" (buffer-string)))
    ))

(new-vanilla lock
  (load-file "~/.emacs.d/lisp/init-utils.el")
  (with-emacs-version>= "24.2"
    (setq create-lockfiles nil)))

(new-vanilla blah
  :depends (foo bar)

  )

(new-vanilla gnus
  (setq gnus-select-method
        '(nnimap "imap.gmail.com"))
  (load "~/CloudStation/Sylvain/emacs/personal.el" :noerror))

(f9 (vanilla-emacs nil "gnus"))


(new-vanilla erc
  (setq erc-autojoin-channels-alist '(("freenode.net" "#emacs"))))

(new-vanilla electric-test
  (electric-indent-mode 1))

(provide 'init-vanilla)
