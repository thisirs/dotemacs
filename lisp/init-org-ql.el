;;; init-org-ql.el --- -*- lexical-binding: t; -*-

;; https://github.com/alphapapa/org-ql
(use-package org-ql                     ; Org Query Language, search command, and agenda-like view
  :preface
  (autoload-after org-ql-projects org-ql)
  :config
  (defvar org-ql-projects-sources
    `((inbox ,(expand-file-name "Org/someday.org" personal-directory))
      (fixed ,projects-directory)
      (projects ,projects-directory :git t)
      (projects ,(expand-file-name "enseignements/repositories" personal-directory)
                :git t :depth 2)
      (project ,(expand-file-name "conf-files" personal-directory))
      (project "~/.emacs.d")
      (project ,(expand-file-name "anki" personal-directory)
               :files ("todo.org" "queue.org"))
      (projects ,(expand-file-name "recherche/projects" personal-directory)
                :keep t :research t :prefix t)
      (projects ,(expand-file-name "recherche/PoC" personal-directory)
                :keep t :research t :prefix t)
      (ignore "/obsolete/"))
    "Every source of todos shown by `org-ql-projects'.
Each entry is (KIND ARG . PLIST), KIND being one of:

  (inbox FILE)      A loose todo file, shown in the \"Inbox\" group.
  (fixed DIR)       DIR's own todo.org, for housekeeping tasks of a
                    directory that is not a project.  Shown in its own
                    group after Inbox, in the order of this list.
  (project DIR)     A single project rooted at DIR.
  (projects DIR)    Every directory below DIR with a todo.org is a
                    project.  The search stops at the first such
                    directory, so a project's subdirectories are never
                    projects themselves.
  (ignore REGEXP)   Drop the projects whose root matches REGEXP.

Projects, from `project' or `projects' entries, are shown after the
fixed groups, most recently active first.  Their PLIST can hold:

  :files LIST   Org files of the project, relative to its root and
                possibly wildcards.  Defaults to (\"todo.org\").
  :keep t       Show the project even with no open todo.
  :research t   Include it in the research view, see `org-ql-projects'.

and for `projects' entries only:

  :git t        Only count directories that are Git repositories.
  :depth N      How many levels below DIR to look, 1 by default.
  :prefix t     Name projects \"DIR-NAME/SUB\" instead of \"SUB\".

A project whose todo.org is tagged `closed' (via #+FILETAGS:) is not
listed, which is how a finished project with :keep goes away.")

  (defun org-ql-projects--closed-p (root)
    "Non-nil if ROOT's todo.org is tagged `closed' via #+FILETAGS:."
    (let ((file (expand-file-name "todo.org" root)))
      (and (file-readable-p file)
           (with-temp-buffer
             (insert-file-contents file)
             (let ((case-fold-search t))
               (re-search-forward "^#\\+FILETAGS:.*:closed:" nil t))))))

  (defun org-ql-projects--scan (dir depth git)
    "Project roots below DIR, at most DEPTH levels down.
A directory is a project when it has a todo.org and, with GIT
non-nil, is a Git repository.  The search does not descend into a
project, nor into any Git repository."
    (when (file-directory-p dir)
      (mapcan (lambda (sub)
                (let ((sub (file-name-as-directory sub)))
                  (cond ((and (file-exists-p (expand-file-name "todo.org" sub))
                              (or (not git) (file-exists-p (expand-file-name ".git" sub))))
                         (list sub))
                        ((and (> depth 1)
                              (not (file-exists-p (expand-file-name ".git" sub))))
                         (org-ql-projects--scan sub (1- depth) git)))))
              (seq-filter #'file-directory-p (directory-files dir t "\\`[^.]")))))

  (defun org-ql-projects--make (root name plist)
    "Project record for ROOT, displayed as NAME, with options PLIST.
It is a plist with :root, :name, :files (existing absolute paths,
see `org-ql-projects-sources'), :keep and :research."
    (list :root root
          :name name
          :files (delete-dups
                  (mapcan (lambda (file)
                            (file-expand-wildcards (expand-file-name file root)))
                          (or (plist-get plist :files) '("todo.org"))))
          :keep (plist-get plist :keep)
          :research (plist-get plist :research)))

  (defun org-ql-projects--projects (kinds)
    "Project records from the `org-ql-projects-sources' entries of KINDS.
KINDS is a list of entry kinds, such as (project projects).  Projects
with no existing file, ignored, or closed are left out."
    (let ((ignore (mapcar #'cadr (seq-filter (lambda (entry) (eq (car entry) 'ignore))
                                             org-ql-projects-sources)))
          projects)
      (pcase-dolist (`(,kind ,arg . ,plist) org-ql-projects-sources)
        (when (memq kind kinds)
          (let ((dir (file-name-as-directory (expand-file-name arg))))
            (pcase kind
              ((or 'project 'fixed)
               (push (org-ql-projects--make
                      dir (file-name-nondirectory (directory-file-name dir)) plist)
                     projects))
              ('projects
               (let ((base (if (plist-get plist :prefix)
                               (file-name-directory (directory-file-name dir))
                             dir)))
                 (dolist (root (org-ql-projects--scan dir (or (plist-get plist :depth) 1)
                                                      (plist-get plist :git)))
                   (push (org-ql-projects--make
                          root (directory-file-name (substring root (length base))) plist)
                         projects))))))))
      (seq-filter (lambda (project)
                    (let ((root (plist-get project :root)))
                      (and (plist-get project :files)
                           (not (seq-some (lambda (re) (string-match-p re root)) ignore))
                           (not (org-ql-projects--closed-p root)))))
                  (seq-uniq (nreverse projects)
                            (lambda (a b) (equal (plist-get a :root) (plist-get b :root)))))))


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

  (defun org-ql-projects--group-spec (project)
    "Super-group spec matching the org files of PROJECT, a project record.
See `org-ql-projects--make'.  The group matches the files' full
paths rather than the root prefix, so a project nested inside
another cannot swallow its items.

The group name carries the root as the text property
`org-ql-projects-root'.  `org-super-agenda--make-agenda-header'
copies the name's properties onto the header line, which is what
`org-ql-projects-dired' reads.  Since the super-group specs are
saved buffer-locally, this survives refreshing the view.

With :keep, the group is shown even with no todo: the name also
carries the project's files as `org-ql-projects-keep', which tells
`org-ql-projects--add-placeholders' to feed the group a placeholder
item, and the group matches that item too."
    (let ((root (plist-get project :root))
          (files (plist-get project :files))
          (keep (plist-get project :keep)))
      (append
       (list :name (propertize (concat (plist-get project :name)
                                       (org-ql-projects--git-status root))
                               'org-ql-projects-root root
                               'org-ql-projects-keep (and keep files)
                               'mouse-face 'highlight
                               'follow-link t
                               'help-echo (format "mouse-1: Dired %s" root))
             :file-path (mapcar #'regexp-quote files))
       (when keep
         ;; A quoted lambda, not a closure: `org-super-agenda' names
         ;; :pred groups by pattern-matching on `(lambda . ,_)'.
         (list :pred `(lambda (item)
                        (equal (get-text-property 0 'org-ql-projects-placeholder item)
                               ,root)))))))

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
a placeholder only if no item comes from its project's files.
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
               (keep (and (stringp name)
                          (get-text-property 0 'org-ql-projects-keep name)))
               (root (and keep (get-text-property 0 'org-ql-projects-root name))))
          (when (and root
                     (not (seq-intersection keep (mapcar #'expand-file-name files))))
            (push (org-ql-projects--placeholder root) placeholders))))
      (funcall fn (append all-items (nreverse placeholders)))))

  (advice-add 'org-super-agenda--group-items :around #'org-ql-projects--add-placeholders)

  (defun org-ql-projects--groups (projects)
    "Super-group specs for PROJECTS, most recently active first."
    (mapcar #'cdr
            (sort (mapcar (lambda (project)
                            (cons (org-ql-projects--activity (plist-get project :root))
                                  (org-ql-projects--group-spec project)))
                          projects)
                  (lambda (a b) (> (car a) (car b))))))

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

  (defun org-ql-projects (&optional research)
    "Show the todos of every source in `org-ql-projects-sources'.
Groups run Inbox, then the fixed groups in the order given, then the
projects, most recently active first.  Files tagged `noagenda' (via
#+FILETAGS:) are skipped.

With RESEARCH (interactively, a prefix argument), only show todos
tagged `research' or from projects with :research.  The other
projects drop out."
    (interactive "P")
    (let* ((inbox (seq-filter #'file-exists-p
                              (mapcar (lambda (entry) (expand-file-name (cadr entry)))
                                      (seq-filter (lambda (entry) (eq (car entry) 'inbox))
                                                  org-ql-projects-sources))))
           (fixed (org-ql-projects--projects '(fixed)))
           (projects (org-ql-projects--projects '(project projects)))
           (research-files (mapcan (lambda (project)
                                     (and (plist-get project :research)
                                          (copy-sequence (plist-get project :files))))
                                   projects)))
      (org-ql-search (delete-dups (append inbox
                                          (mapcan (lambda (project)
                                                    (copy-sequence (plist-get project :files)))
                                                  (append fixed projects))))
        (if research
            `(and (todo) (not (tags "noagenda"))
                  (or (tags "research")
                      ,@(when research-files
                          `((path ,@(mapcar #'regexp-quote research-files))))))
          '(and (todo) (not (tags "noagenda"))))
        :title (and research "Research projects")
        :super-groups (append (list (list :name "Inbox"
                                          :file-path (mapcar #'regexp-quote inbox)))
                              (mapcar #'org-ql-projects--group-spec fixed)
                              (org-ql-projects--groups projects)))))

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
