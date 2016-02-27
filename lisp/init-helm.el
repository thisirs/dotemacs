(setq helm-command-prefix-key "C-x C-a")
(setq helm-ff-transformer-show-only-basename nil)

(require 'helm)
(require 'helm-help)
(require 'helm-config)
(require 'helm-files)

(use-package helm-descbinds
  :config
  (require 'helm-config)
  (helm-descbinds-mode))

(helm-mode)

(global-set-key (kbd "C-x C-m") #'helm-M-x)

(global-set-key (kbd "C-x r b") #'helm-bookmarks)

(global-set-key (kbd "M-y") #'helm-show-kill-ring)

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
  '("~/CloudStation/Sylvain/recherche/biblio/"
    "~/CloudStation/Sylvain/recherche/biblio/tracking/"))

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

(define-helm-type-attribute 'file
  `((action
     ("Find file" . helm-find-many-files)
     ("Find file as root" . helm-find-file-as-root)
     ("Store org link of file" . helm-org-store-link)
     ("Open dired in file's directory" . helm-open-dired)
     ("Open file externally (C-u to choose)" . helm-open-file-externally)
     ("Open file with default tool" . helm-open-file-with-default-tool))
    (persistent-help . "Show this file")
    (action-transformer helm-transform-file-load-el
                        helm-transform-file-browse-url)
    (candidate-transformer helm-highlight-files
                           helm-w32-pathname-transformer
                           helm-skip-boring-files))
  "File name.")

(defvar helm-ebooks-path
  '("~/Downloads/Scientific_Ebooks/"
    "~/Downloads/Books/"
    "~/CloudStation/Sylvain/manuals/"
    "~/CloudStation/Sylvain/books/"))

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

(provide 'init-helm)
