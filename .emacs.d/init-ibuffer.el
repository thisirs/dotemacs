;;; IBuffer
(require 'ibuffer)

(defadvice ibuffer-diff-with-file (around ibuffer-diff-two-buffers activate)
  (require 'diff)
  (let ((marked-bufs (ibuffer-get-marked-buffers)))
    (if (eq (length marked-bufs) 2)
        (diff (car marked-bufs) (cadr marked-bufs))
      ad-do-it)))

;; don't show empty groups
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

(defadvice ibuffer (around ibuffer-point-to-most-recent activate)
  "Open ibuffer with cursour pointed to second most recent buffer
name"
  (let ((recent-buffer-name (buffer-name)))
    ad-do-it
    (ibuffer-jump-to-buffer recent-buffer-name)
    (ibuffer-next-buffer)))


(defun find-projects (dir)
  "Return a list of all directories and sub-directories
containing a not hidden git repository."
  (let* ((dir (file-name-as-directory dir))
         (list
          (and (file-exists-p (concat dir "/.git"))
               (not (file-exists-p (concat dir "/.hidden")))
               (cons dir nil))))
    (apply 'append list
           (mapcar
            (lambda (path)
              (if (file-directory-p path)
                  (find-projects path)))
            ;; avoiding . and ..
            (directory-files dir t "[^\\.]\\|\\(\\.\\{3,\\}\\)")))))


(defun make-ibuffer-projects-list (prefix &rest dir-list)
  "Return a list whose elements are of the form ((`prefix' (filename . `directory')"
  (mapcar
   (lambda (dir)
     (list (concat prefix (file-name-nondirectory
                           (directory-file-name dir)))
           `(filename . ,dir)))
   (apply 'append
          (mapcar
           (lambda (dir)
             (and (file-directory-p dir)
                  (nreverse (find-projects dir))))
           dir-list))))

(setq ibuffer-project-alist
      `(("Project: " . ,(concat (getenv "HOME") "/dotemacs/dotemacs"))
        ("Project: " . ,(concat (getenv "HOME") "/repositories/dotemacs"))
        ("Project on THISKEY: " . "/media/THISKEY/programming")
        ( "" . "/media/THISKEY/Documents/These/")))

(setq ibuffer-project-list-cache-file
      "~/.emacs.d/cache/ibuffer-project")

(defun ibuffer-project-list-write-cache (ibuffer-project-list)
  "Write `ibuffer-project-list' in cache file. Return `ibuffer-project-list'."
  (with-temp-buffer
    (print ibuffer-project-list (current-buffer))
    (write-region (point-min)
                  (point-max)
                  ibuffer-project-list-cache-file))
  ibuffer-project-list)

(defun ibuffer-project-list-read-cache ()
  "Read the cache file if it exists; otherwise return nil."
  (if (file-exists-p ibuffer-project-list-cache-file)
      (with-temp-buffer
        (insert-file-contents ibuffer-project-list-cache-file)
        (read (buffer-string)))))

(defun ibuffer-project-list-generate ()
  "Generate project list by examining `ibuffer-project-alist'."
  (mapcan
   (lambda (e)
     (make-ibuffer-projects-list (car e) (cdr e)))
   ibuffer-project-alist))

(defun ibuffer-project-list ()
  "Return project list. Generates and caches it if necessary."
  (or (ibuffer-project-list-read-cache)
      (ibuffer-project-list-write-cache
       (ibuffer-project-list-generate))))

(defun ibuffer-project-generate-and-cache ()
  (interactive)
  (ibuffer-project-list-write-cache
   (ibuffer-project-list-generate))
  (minibuffer-message "IBuffer cache written!"))

;; Generate project list when idle
(run-with-idle-timer 10 nil 'ibuffer-project-generate-and-cache)

(setq ibuffer-saved-filter-groups
      `(("default"
         ,@(ibuffer-project-list)
         ,@(load-file-to-list "~/Dropbox/emacs/ibuffer-personnal.el")
         ("Org"
          (mode . org-mode))
         ("TeX/LaTeX"
          (or
           (mode . latex-mode)
           (name . "\\.bib$")
           (name . "\\.tex$")))
         ("Mail"
          (or
           (mode . message-mode)
           (mode . mail-mode)
           ))
         ("Dired"
          (mode . dired-mode)
          )
         ("THISKEY's programming"
          (filename . "/media/THISKEY/programming/"))
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
           (name . "^\\*MATLAB\\*$"))))
        ;; other filter groups
        ,@(load-file-to-list "~/Dropbox/emacs/ibuffer-filter-groups.el")))

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
