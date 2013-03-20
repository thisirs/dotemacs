(setq helm-command-prefix-key "C-x C-a")
(setq helm-ff-transformer-show-only-basename nil)

(require 'helm)
(require 'helm-help)
(require 'helm-config)
(require 'helm-files)

;; (helm-read-string-mode 1)
;; (ac-mode -1)

(global-set-key (kbd "C-x C-m") 'helm-M-x)

(global-set-key (kbd "C-x r b") 'helm-bookmarks)

(setq helm-su-or-sudo "sudo")

(setq helm-c-locate-command "locate -e -b %s -r \"%s\"")

;; Don't save history information to file
(remove-hook 'kill-emacs-hook 'helm-c-adaptive-save-history)

;; Make `helm-for-files-preferred-list' dynamic
(defadvice helm-for-files (around update-helm-list activate)
  (let ((helm-for-files-preferred-list
         (helm-for-files-update-list)))
    ad-do-it))

(defun helm-for-files-update-list ()
  `(helm-c-source-buffers-list
    helm-c-source-recentf
    helm-c-source-bookmarks
    helm-c-source-file-cache
    helm-c-source-files-in-current-dir
    ,@(if (or (file-exists-p "/media/THISKEY")
              (file-exists-p "/run/media/sylvain/THISKEY"))
          '(helm-c-source-locate-thiskey))
    helm-c-source-locate))

(defun helm-c-locate-thiskey-init ()
  "Initialize async locate process for `helm-c-source-locate'."
  (start-process-shell-command
   "locate-thiskey-process" nil
   (format helm-c-locate-command
           (concat "-d " (expand-file-name "~/.locate.db") " -i")
           helm-pattern)))

(defvar helm-c-source-locate-thiskey
  '((name . "Locate in THISKEY")
    (init . helm-locate-set-command)
    (candidates-process . helm-c-locate-thiskey-init)
    (type . file)
    (requires-pattern . 3)
    (history . ,'helm-file-name-history)
    (keymap . ,helm-generic-files-map)
    (help-message . helm-generic-file-help-message)
    (candidate-number-limit . 9999)
    (mode-line . helm-generic-file-mode-line-string)
    (delayed))
  "Find files matching the current input pattern with locate.")

;; Helm for searching manuals
(defvar helm-c-manual-path
  '("~/Documents/manuals/"
    ("/media/THISKEY/Documents/Manuals/"
     "~/.backup/THISKEY/Documents/Manuals/")
    ("/media/THISKEY/Documents/latex/package_manuals"
     "~/.backup/THISKEY/Documents/latex/package_manuals")
    ("/media/THISKEY/Documents/latex/beamer/"
     "~/.backup/THISKEY/Documents/latex/beamer/")
    ("/media/THISKEY/Documents/latex/tikz/"
     "~/.backup/THISKEY/Documents/latex/tikz/")
    ("/media/THISKEY/Documents/latex/tex/"
     "~/.backup/THISKEY/Documents/latex/tex/")
    ("/media/KROKEY/LYCEE/Ressources/TiKz/"
     "~/.backup/KROKEY/LYCEE/Ressources/TiKz/"))
  "List of path to look for manuals. Each element is either a
  string or a list of string containing fallback directories.")


(defun helm-c-manual-path ()
  "Returns all existing directories containing manuals."
  (delq nil
        (mapcar
         (lambda (paths)
           (let ((paths (if (listp paths) paths (list paths))) path found)
             (while (and (not found) paths)
               (setq path (pop paths))
               (setq found (file-exists-p path)))
             (and found path)))
         helm-c-manual-path)))

(defvar helm-c-manual-regexp "\\.pdf\\'")

(defun helm-manual-get-candidates ()
  "Collect manuals found in paths `helm-c-manual-path'."
  (mapcan (lambda (path)
            (and (file-directory-p path)
                 (directory-files path t helm-c-manual-regexp)))
          (helm-c-manual-path)))

(defun helm-c-manual-transformer (files sources)
  (mapcar 'file-name-nondirectory files))

(defvar helm-c-source-manual
  `((name ."Manuals")
    (candidates . helm-manual-get-candidates)
    (real-to-display . file-name-nondirectory)
    (type . file)))

(defun helm-manual ()
  (interactive)
  (helm :sources 'helm-c-source-manual
        :buffer "*Helm manuals*"
        :prompt "Manuals: "))

(define-key helm-command-map (kbd "h m") 'helm-manual)

(defvar helm-bib-locations
  '("~/Dropbox/These/bib/bregman"))

;; Open files with evince instead of xpdf
(setq helm-c-pdfgrep-default-read-command "evince -p %p \"%f\"")

(setq helm-c-pdfgrep-default-command "pdfgrep -C 300 --color never -niH \"%s\" %s")

(defun helm-search-bib ()
  (interactive)
  (let ((helm-c-grep-default-function 'helm-c-pdfgrep-init))
    (helm-do-pdfgrep-1 helm-bib-locations)))

(define-key helm-command-map (kbd "h b") 'helm-search-bib)

(defun helm-org-store-link (candidate)
  (setq org-stored-links
        (cons (list (concat "file:" candidate)) org-stored-links)))

(define-helm-type-attribute 'file
  `((action
     ("Find file" . helm-find-many-files)
     ("Find file as root" . helm-find-file-as-root)
     ("Store org link of file" . helm-org-store-link)
     ("Open dired in file's directory" . helm-c-open-dired)
     ("Open file externally (C-u to choose)" . helm-c-open-file-externally)
     ("Open file with default tool" . helm-c-open-file-with-default-tool))
    (persistent-help . "Show this file")
    (action-transformer helm-c-transform-file-load-el
                        helm-c-transform-file-browse-url)
    (candidate-transformer helm-c-highlight-files
                           helm-c-w32-pathname-transformer
                           helm-c-skip-boring-files))
  "File name.")

(provide 'init-helm)
