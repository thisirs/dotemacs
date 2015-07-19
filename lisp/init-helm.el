(setq helm-command-prefix-key "C-x C-a")
(setq helm-ff-transformer-show-only-basename nil)

(require 'helm)
(require 'helm-help)
(require 'helm-config)
(require 'helm-files)

(require 'helm-descbinds)
(helm-descbinds-mode)

(helm-mode)

(global-set-key (kbd "C-x C-m") 'helm-M-x)

(global-set-key (kbd "C-x r b") 'helm-bookmarks)

(global-set-key (kbd "M-y") 'helm-show-kill-ring)

(setq helm-su-or-sudo "sudo")

(setq helm-locate-command "locate -e -b %s -r %s")

(setq helm-quick-update t
      helm-idle-delay 0.01
      helm-input-idle-delay 0.01)

;; Don't save history information to file
(remove-hook 'kill-emacs-hook 'helm-adaptive-save-history)

;; Make `helm-for-files-preferred-list' dynamic
(defadvice helm-for-files (around update-helm-list activate)
  (let ((helm-for-files-preferred-list
         (helm-for-files-update-list)))
    ad-do-it))

(defun helm-for-files-update-list ()
  `(helm-source-buffers-list
    helm-source-recentf
    helm-source-bookmarks
    helm-source-file-cache
    helm-source-files-in-current-dir
    helm-source-locate))

;; Helm for searching manuals
(defvar helm-manual-path
  '("~/CloudStation/Sylvain/manuals/"
    "~/CloudStation/Sylvain/manuals/beamer/"
    "~/CloudStation/Sylvain/manuals/latex/"
    "~/CloudStation/Sylvain/manuals/pgf-tikz/"
    "~/CloudStation/Sylvain/manuals/refcards/"
    "~/CloudStation/Sylvain/books/")
  "List of path to look for manuals. Each element is either a
  string or a list of string containing fallback directories.")

(defun helm-manual-path ()
  "Returns all existing directories containing manuals."
  (delq nil
        (mapcar
         (lambda (paths)
           (let ((paths (if (listp paths) paths (list paths))) path found)
             (while (and (not found) paths)
               (setq path (pop paths))
               (setq found (file-exists-p path)))
             (and found path)))
         helm-manual-path)))

(defvar helm-manual-regexp "\\.pdf\\'")

(defun helm-manual-get-candidates ()
  "Collect manuals found in paths `helm-manual-path'."
  (mapcan (lambda (path)
            (and (file-directory-p path)
                 (directory-files path t helm-manual-regexp)))
          (helm-manual-path)))

(defun helm-manual-transformer (files sources)
  (mapcar 'file-name-nondirectory files))

(defvar helm-source-manual
  `((name ."Manuals")
    (candidates . helm-manual-get-candidates)
    (real-to-display . file-name-nondirectory)
    (type . file)))

(defun helm-manual ()
  (interactive)
  (helm :sources 'helm-source-manual
        :buffer "*Helm manuals*"
        :prompt "Manuals: "))

(define-key helm-command-map (kbd "h m") 'helm-manual)
(define-key helm-command-map (kbd "f") 'helm-for-files)

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

(define-key helm-command-map (kbd "h p") 'helm-search-bib)

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

(provide 'init-helm)
