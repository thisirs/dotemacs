(setq helm-command-prefix-key "C-x C-a")
(setq helm-ff-transformer-show-only-basename t)

;; History first in sources list
(setq helm-mode-reverse-history t)


(require 'helm)
(require 'helm-help)
(require 'helm-config)
(require 'helm-files)

;; https://github.com/emacs-helm/helm-descbinds
(use-package helm-descbinds             ; A convenient `describe-bindings' with `helm'
  :config
  (require 'helm-config)
  (helm-descbinds-mode))

(helm-mode)

(setq helm-M-x-fuzzy-match t)
(keymap-global-set "C-x C-m" #'helm-M-x)

(keymap-global-set "C-x r b" #'helm-bookmarks)

(keymap-global-set "M-y" #'helm-show-kill-ring)

;; Show input in header line
(setq helm-echo-input-in-header-line t)

;; Remove header lines if only a single source
(setq helm-display-header-line nil)

(setq helm-su-or-sudo "sudo")

(setq helm-quick-update t
      helm-idle-delay 0.01
      helm-input-idle-delay 0.01)

;; Don't save history information to file
(remove-hook 'kill-emacs-hook 'helm-adaptive-save-history)

;; ;; Make `helm-for-files-preferred-list' dynamic
;; (defadvice helm-for-files (around update-helm-list activate)
;;   (let ((helm-for-files-preferred-list
;;          (helm-for-files-update-list)))
;;     ad-do-it))

;; (defun helm-for-files-update-list ()
;;   `(helm-source-buffers-list
;;     helm-source-recentf
;;     helm-source-bookmarks
;;     helm-source-file-cache
;;     helm-source-files-in-current-dir
    ;; helm-source-locate))

(define-key helm-command-map (kbd "f") #'helm-for-files)

(defvar helm-bib-locations
  '("~/SynologyDrive/Sylvain/recherche/biblio/"
    "~/SynologyDrive/Sylvain/recherche/biblio/tracking/"))

;; Open files with evince instead of xpdf
(setq helm-pdfgrep-default-read-command "evince -p %p \"%f\"")

(setq helm-pdfgrep-default-command "pdfgrep -C 300 --color never -niH \"%s\" %s")

(defun helm-search-bib ()
  (interactive)
  (let ((helm-grep-default-function 'helm-pdfgrep-init))
    (helm-do-pdfgrep-1 helm-bib-locations)))

(define-key helm-command-map (kbd "h p") #'helm-search-bib)

(defun helm-org-store-link (candidate)
  (setq org-stored-links
        (cons (list (concat "file:" candidate)) org-stored-links)))

(defvar helm-ebooks-path
  '("~/Downloads/Scientific_Ebooks/"
    "~/Downloads/Books/"
    "~/SynologyDrive/Sylvain/manuals/"
    "~/SynologyDrive/Sylvain/books/"))

(defun helm-find-ebooks ()
  (interactive)
  (helm :sources
        (mapcar (lambda (dir)
                  (helm-build-async-source "Find"
                    :header-name `(lambda (name)
                                    (concat name " in [" ,dir "]"))
                    :candidates-process `(lambda ()
                                           (let ((default-directory ,dir))
                                             (funcall 'helm-find-shell-command-fn)))
                    :filtered-candidate-transformer 'helm-findutils-transformer
                    :action-transformer 'helm-transform-file-load-el
                    :action 'helm-type-file-actions
                    :keymap helm-generic-files-map
                    :candidate-number-limit 9999
                    :requires-pattern 3))
                helm-ebooks-path)
        :buffer "*helm find ebooks*"
        :ff-transformer-show-only-basename t
        :case-fold-search helm-file-name-case-fold-search))

(define-key helm-command-map (kbd "h m") #'helm-find-ebooks)

;; Error after opening Rnw files
;; See https://github.com/kaz-yos/emacs/blob/master/init.d/200_emacsclient-related.el#L63
(defun remove-helm-functions ()
  (remove-hook 'post-command-hook 'helm--maybe-update-keymap)
  (remove-hook 'post-command-hook 'helm--update-header-line))
(add-hook 'pre-command-hook 'remove-helm-functions)
(provide 'init-helm)
