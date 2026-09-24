;;; init-org-ql.el --- -*- lexical-binding: t; -*-

;; https://github.com/alphapapa/org-ql
(use-package org-ql                     ; Org Query Language, search command, and agenda-like view
  :preface
  (autoload-after org-ql-projects org-ql)
  :config
  (defvar org-ql-projects-files
    (list ;(expand-file-name "Org/agenda.org" personal-directory)
          (expand-file-name "Org/someday.org" personal-directory))
    "Loose todo files, not attached to any project.")

  (defvar org-ql-projects-directories
    (mapcar (lambda (dir) (file-name-as-directory (expand-file-name dir)))
            (list projects-directory
                  (expand-file-name "enseignements/repositories" personal-directory)
                  (expand-file-name "conf-files" personal-directory)
                  (expand-file-name "~/.emacs.d")))
    "Directories holding my projects.
A known project counts as mine when its root is one of these or
lies below one.")

  (defvar org-ql-projects-research-directories
    (mapcar (lambda (dir) (file-name-as-directory (expand-file-name dir personal-directory)))
            (list "recherche/projects" "recherche/PoC"))
    "Directories whose every subdirectory is a research project.
Unlike projects found through `org-ql-projects-directories', one is
listed as soon as it has a todo.org, even with no open todo, until
that todo.org is tagged `closed' (via #+FILETAGS:).  One without a
todo.org is not listed at all, which is what hides the finished
projects that predate this.  They need not be known to project.el.")

  (defvar org-ql-projects-ignore-regexps '("/obsolete/")
    "Regexps matched against project roots to exclude them.
Projects outside `org-ql-projects-directories' are already excluded.")

  (defvar org-ql-projects-pseudo-roots
    (list (expand-file-name projects-directory))
    "Directories with a todo.org that are not projects.
Unlike `org-ql-projects-directories', these can be anywhere: each
holds housekeeping tasks for that directory itself rather than for a
project inside it, so it gets its own group instead of being folded
into `org-ql-projects-files'.  Listed between Inbox and the sorted
projects, in the order given here.")

  (defun org-ql-projects--roots ()
    "Known project roots in `org-ql-projects-directories', minus ignored ones."
    (seq-filter (lambda (root)
                  (and (seq-some (lambda (dir) (string-prefix-p dir root))
                                 org-ql-projects-directories)
                       (not (seq-some (lambda (re) (string-match-p re root))
                                      org-ql-projects-ignore-regexps))))
                (mapcar #'expand-file-name (project-known-project-roots))))

  (defun org-ql-projects--closed-p (root)
    "Non-nil if ROOT's todo.org is tagged `closed' via #+FILETAGS:."
    (let ((file (expand-file-name "todo.org" root)))
      (and (file-readable-p file)
           (with-temp-buffer
             (insert-file-contents file)
             (let ((case-fold-search t))
               (re-search-forward "^#\\+FILETAGS:.*:closed:" nil t))))))

  (defun org-ql-projects--research-roots ()
    "Research project roots with a todo.org not tagged `closed'.
See `org-ql-projects-research-directories'."
    (seq-remove #'org-ql-projects--closed-p
                (mapcar #'file-name-as-directory
                        (seq-filter (lambda (dir)
                                      (file-exists-p (expand-file-name "todo.org" dir)))
                                    (mapcan (lambda (dir)
                                              (when (file-directory-p dir)
                                                (directory-files dir t "\\`[^.]")))
                                            org-ql-projects-research-directories)))))

  (defun org-ql-projects--activity (root)
    "Last-activity time of project ROOT, in seconds since the epoch.
The date of the last Git commit, falling back to todo.org's mtime
for projects that are not repositories, and to ROOT's own mtime
when there is no todo.org either."
    (or (let ((default-directory root))
          (ignore-errors
            (string-to-number (car (process-lines "git" "log" "-1" "--format=%ct")))))
        (float-time (file-attribute-modification-time
                     (file-attributes
                      (let ((todo (expand-file-name "todo.org" root)))
                        (if (file-exists-p todo) todo root)))))))

  (defun org-ql-projects--name (root)
    "Display name for ROOT: its path below its `org-ql-projects-directories' base.
Nested projects such as the AOS1/AOS2 teaching repositories would
otherwise all show up as bare, ambiguous names like \"Lectures\".
Projects in `org-ql-projects-research-directories' keep that
directory's name as a prefix, as in \"PoC/Some_Idea\"."
    (let ((base (seq-find (lambda (dir) (string-prefix-p dir root))
                          (append org-ql-projects-directories
                                  (mapcar (lambda (dir)
                                            (file-name-directory (directory-file-name dir)))
                                          org-ql-projects-research-directories)))))
      (directory-file-name
       (if (and base (> (length root) (length base)))
           (substring root (length base))
         (file-name-nondirectory (directory-file-name root))))))

  (defun org-ql-projects--git-status (root)
    "Git status of project ROOT, as a string to append to its name.
Empty for a clean repository, and for a project that is not one at
all.  Otherwise the counts `git status --porcelain' reports, in that
order: staged files after a plus sign, modified but unstaged ones
after an asterisk, untracked ones after a question mark, then how far
the branch is ahead of and behind its upstream, after ↑ and ↓."
    (let ((default-directory root)
          (staged 0) (unstaged 0) (untracked 0)
          branch lines parts)
      (setq lines (ignore-errors
                    (process-lines "git" "--no-optional-locks" "status"
                                   "--porcelain" "--branch")))
      (setq branch (car lines))
      (dolist (line (cdr lines))
        (if (string-prefix-p "??" line)
            (setq untracked (1+ untracked))
          (unless (eq (aref line 0) ?\s)
            (setq staged (1+ staged)))
          (unless (eq (aref line 1) ?\s)
            (setq unstaged (1+ unstaged)))))
      (when (> staged 0) (push (format "+%d" staged) parts))
      (when (> unstaged 0) (push (format "*%d" unstaged) parts))
      (when (> untracked 0) (push (format "?%d" untracked) parts))
      ;; The --branch line reads like "## master...origin/master
      ;; [ahead 1, behind 2]"; either half of the bracket can be missing,
      ;; as can the bracket and the upstream themselves.
      (when (and branch (string-match "ahead \\([0-9]+\\)" branch))
        (push (concat "↑" (match-string 1 branch)) parts))
      (when (and branch (string-match "behind \\([0-9]+\\)" branch))
        (push (concat "↓" (match-string 1 branch)) parts))
      (if parts
          (concat "  " (string-join (nreverse parts) " "))
        "")))

  (defun org-ql-projects--group-spec (root &optional keep)
    "Super-group spec matching ROOT's todo.org.
The group matches the file's full path rather than the root prefix,
so a project nested inside another cannot swallow its items.

The group name carries ROOT as the text property
`org-ql-projects-root'.  `org-super-agenda--make-agenda-header'
copies the name's properties onto the header line, which is what
`org-ql-projects-dired' reads.  Since the super-group specs are
saved buffer-locally, this survives refreshing the view.

With KEEP non-nil, the group is shown even with no todo: the name
also carries `org-ql-projects-keep', which tells
`org-ql-projects--add-placeholders' to feed the group a placeholder
item, and the group matches that item too."
    (append
     (list :name (propertize (concat (org-ql-projects--name root)
                                     (org-ql-projects--git-status root))
                             'org-ql-projects-root root
                             'org-ql-projects-keep keep
                             'mouse-face 'highlight
                             'follow-link t
                             'help-echo (format "mouse-1: Dired %s" root))
           :file-path (regexp-quote (expand-file-name "todo.org" root)))
     (when keep
       ;; A quoted lambda, not a closure: `org-super-agenda' names
       ;; :pred groups by pattern-matching on `(lambda . ,_)'.
       (list :pred `(lambda (item)
                      (equal (get-text-property 0 'org-ql-projects-placeholder item)
                             ,root))))))

  (defun org-ql-projects--placeholder (root)
    "Agenda line standing in for the missing todos of project ROOT.
It carries the header's keymap, so RET or a click on it opens Dired
on ROOT like on the header itself."
    (propertize "  (no open todo)"
                'face 'shadow
                'org-ql-projects-placeholder root
                'org-ql-projects-root root
                'keymap org-super-agenda-header-map
                'mouse-face 'highlight))

  (defun org-ql-projects--add-placeholders (fn all-items)
    "Call FN on ALL-ITEMS plus placeholders for the empty kept groups.
Around advice for `org-super-agenda--group-items', which drops
empty groups.  A group is kept when its name carries
`org-ql-projects-keep' (see `org-ql-projects--group-spec'); it gets
a placeholder only if no item comes from its project's todo.org.
Doing it here rather than in `org-ql-projects' is what makes it
survive refreshing the view."
    (let ((files (delete-dups
                  (delq nil (mapcar (lambda (item)
                                      (when-let* ((marker (or (get-text-property 0 'org-marker item)
                                                              (get-text-property 0 'org-hd-marker item)))
                                                  (buffer (marker-buffer marker)))
                                        (buffer-file-name buffer)))
                                    all-items))))
          placeholders)
      (dolist (group (and (listp org-super-agenda-groups) org-super-agenda-groups))
        (let* ((name (plist-get group :name))
               (root (and (stringp name)
                          (get-text-property 0 'org-ql-projects-keep name)
                          (get-text-property 0 'org-ql-projects-root name))))
          (when (and root
                     (not (member (expand-file-name "todo.org" root)
                                  (mapcar #'expand-file-name files))))
            (push (org-ql-projects--placeholder root) placeholders))))
      (funcall fn (append all-items (nreverse placeholders)))))

  (advice-add 'org-super-agenda--group-items :around #'org-ql-projects--add-placeholders)

  (defun org-ql-projects--groups (roots &optional keep-roots)
    "Super-group specs for ROOTS, most recently active first.
Those also in KEEP-ROOTS are shown even with no todo."
    (let ((decorated (mapcar (lambda (root)
                               (cons (org-ql-projects--activity root) root))
                             roots)))
      (mapcar (lambda (cell)
                (org-ql-projects--group-spec (cdr cell)
                                             (and (member (cdr cell) keep-roots) t)))
              (sort decorated (lambda (a b) (> (car a) (car b)))))))

  (defun org-ql-projects--pseudo-groups (roots)
    "Super-group specs for pseudo-project ROOTS, in the order given.
Unlike `org-ql-projects--groups', these are not sorted by activity:
pseudo-projects are not projects, so \"most recently active\" is not
a meaningful order for them.  See `org-ql-projects-pseudo-roots'."
    (mapcar #'org-ql-projects--group-spec roots))

  (defun org-ql-projects-dired ()
    "Open Dired on the project of the group header at point."
    (interactive)
    (let ((root (org-get-at-bol 'org-ql-projects-root)))
      (unless root
        (user-error "No project attached to this line"))
      (dired root)))

  (defun org-ql-projects-dired-mouse (event)
    "Open Dired on the project of the group header clicked in EVENT.
Off a project header, fall back to `org-agenda-goto-mouse'.  That
makes this safe to bind in `org-ql-view-map' too, which is what
catches clicks to the right of a header: the header string is
only as wide as its name, so the rest of the line is not covered
by `org-super-agenda-header-map'."
    (interactive "e")
    (mouse-set-point event)
    (if (org-get-at-bol 'org-ql-projects-root)
        (org-ql-projects-dired)
      (org-agenda-goto-mouse event)))

  (defun org-ql-projects-display ()
    "Display the thing at point in another window, without selecting it.
On a project group header, that is Dired on the project; elsewhere,
the entry at point, as `org-agenda-show-and-scroll-up' does."
    (interactive)
    (if-let* ((root (org-get-at-bol 'org-ql-projects-root)))
        (display-buffer (dired-noselect root))
      (call-interactively #'org-agenda-show-and-scroll-up)))

  (org-ql-defpred research-project ()
    "Match entries in a project under `org-ql-projects-research-directories'."
    :body (let ((file (buffer-file-name (buffer-base-buffer))))
            (and file
                 (seq-some (lambda (dir) (string-prefix-p dir (expand-file-name file)))
                           org-ql-projects-research-directories))))

  (defun org-ql-projects (&optional research)
    "Show all todos from every known project's todo.org, plus loose ones.
Groups run Inbox, then the pseudo-projects in
`org-ql-projects-pseudo-roots', then real projects ordered by last
activity, most recent first.  Files tagged `noagenda' (via
#+FILETAGS:) are skipped.  Research projects (see
`org-ql-projects-research-directories') are listed as soon as they
have a todo.org, even with no open todo, unless it is tagged
`closed'.

With RESEARCH (interactively, a prefix argument), only show todos
from research projects or tagged `research'.  The other projects
drop out."
    (interactive "P")
    (let* ((keep-roots (org-ql-projects--research-roots))
           (roots (seq-filter (lambda (root)
                                (file-exists-p (expand-file-name "todo.org" root)))
                              (org-ql-projects--roots)))
           (pseudo-roots (seq-filter (lambda (root)
                                       (file-exists-p (expand-file-name "todo.org" root)))
                                     org-ql-projects-pseudo-roots))
           (todo-files
            (delete-dups (append org-ql-projects-files
                                 (seq-filter #'file-exists-p
                                             (mapcar (lambda (root)
                                                       (expand-file-name "todo.org" root))
                                                     (append pseudo-roots roots keep-roots)))))))
      (org-ql-search todo-files
        (if research
            '(and (todo) (or (tags "research") (research-project))
                  (not (tags "noagenda")))
          '(and (todo) (not (tags "noagenda"))))
        :title (and research "Research projects")
        :super-groups (append (list '(:name "Inbox" :file-path "Sylvain/Org/"))
                              (org-ql-projects--pseudo-groups pseudo-roots)
                              (org-ql-projects--groups (delete-dups (append roots keep-roots))
                                                       keep-roots)))))

  ;; `org-ql-views' lives in org-ql-view, which loading org-ql alone
  ;; does not pull in.  It also pulls in org-super-agenda.
  (require 'org-ql-view)

  ;; `org-super-agenda-header-map' is a text-property keymap, so these
  ;; only take effect with point on a group header.  Headers with no
  ;; project attached ("Inbox", the `:auto-category' groups of
  ;; `org-roam-todo-list') just signal a `user-error'.
  (define-key org-super-agenda-header-map (kbd "RET") #'org-ql-projects-dired)
  (define-key org-super-agenda-header-map [mouse-2] #'org-ql-projects-dired-mouse)
  (define-key org-super-agenda-header-map (kbd "C-o") #'org-ql-projects-display)
  ;; And on the bare part of a header line, past the header string.
  (define-key org-ql-view-map [mouse-2] #'org-ql-projects-dired-mouse)
  ;; Like C-o in Dired or a compilation buffer: show, don't select.
  (define-key org-ql-view-map (kbd "C-o") #'org-ql-projects-display)

  ;; A function view is `call-interactively'd by `org-ql-view', so
  ;; register the command itself rather than duplicating its file list
  ;; into a :buffers-files plist.
  (setf (alist-get "Projects: All todos" org-ql-views nil nil #'string=)
        #'org-ql-projects))

(provide 'init-org-ql)
