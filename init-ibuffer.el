;;; IBuffer

(require 'ibuffer)

(setq ibuffer-saved-filter-groups
      '(("default"
         ("Org"
          (or (mode . org-mode)
              (mode . org-agenda-mode)))
         ("TeX/LaTeX"
          (or
           (mode . latex-mode)
           (name . "\\.bib$")
           (name . "\\.tex$")))
         ("Gnus"
          (or
           (mode . message-mode)
           (mode . mail-mode)
           (mode . gnus-group-mode)
           (mode . bbdb-mode)
           (mode . gnus-summary-mode)
           (mode . gnus-article-mode)))
         ("Dired"
          (mode . dired-mode))
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
         ("Info" (or (mode . Man-mode)
                     (mode . woman-mode)))
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


(when (require 'ibuffer-project nil t)
  (setq ibuffer-project-alist
        '(("Project: %S"
           "~/Dropbox/emacs/site-lisp"
           "~/repositories"
           "~/.emacs.d"
           "~/Dropbox/programming")
          ("%D" "~/Dropbox/scripts")
          ("%D" "~/Dropbox/projects/")
          ("%D" "~/Dropbox/conf-files/")
          ("Boss: %D" "~/Dropbox/These/")))
  (setq ibuffer-project-cache-file
        (expand-file-name "cache/.ibuffer-project.el" user-emacs-directory))
  (ibuffer-project-refresh t))

;; Files that are part of the same project might be in different
;; filter group.
(setq find-file-visit-truename t)

(defadvice ibuffer-diff-with-file (around ibuffer-diff-two-buffers activate)
  (require 'diff)
  (let ((marked-bufs (ibuffer-get-marked-buffers)))
    (if (eq (length marked-bufs) 2)
        (diff (car marked-bufs) (cadr marked-bufs))
      ad-do-it)))

;; Don't show empty groups
(setq ibuffer-show-empty-filter-groups nil)

(defun ibuffer-next-buffer-aux (list)
  (if (null list)
      (error "No buffers!"))
  (let* ((current-buffer (ibuffer-current-buffer t))
         (list1 list)
         next-buffer
         (ibuffer-buffer-list
          (mapcar #'car
                  (ibuffer-current-state-list)))
         (list0 (cdr (member current-buffer list))))
    (while (and list0 (not (memql (car list0) ibuffer-buffer-list)))
      (setq list0 (cdr list0)))
    (ibuffer-jump-to-buffer
     (buffer-name
      (car
       (or list0
           (progn
             (while (not (memql (car list1) ibuffer-buffer-list))
               (setq list1 (cdr list1)))
             list1)))))))

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

(require 'ido)
(ido-mode t)
(setq ido-mode 'buffer)
(setq ido-auto-merge-work-directories-length -1)

(define-key ibuffer-mode-map (kbd "C-s") 'ido-switch-buffer)

(defadvice ibuffer (around ibuffer-point-to-most-recent activate)
  "Open ibuffer with cursor pointed to second most recent buffer
name"
  (let ((recent-buffer-name (buffer-name)))
    ad-do-it
    (ibuffer-jump-to-buffer recent-buffer-name)
    (ibuffer-next-buffer)))

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

(provide 'init-ibuffer)
