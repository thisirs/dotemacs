
(defvar vanilla-features nil)

(defmacro new-vanilla (name &rest body)
  (declare (indent 1) (debug t))
  (add-to-list 'vanilla-features name)
  `(defmacro ,(intern (concat "vanilla-" (symbol-name name))) ()
     '(progn ,@body)))

(defun vanilla-emacs (&rest features)
  (interactive)
  (let* ((form `(progn ,@(mapcar
                          (lambda (feature)
                            (macroexpand `(,feature)))
                          features)))
         (buf (generate-new-buffer "*emacs*"))
         (program-args (list "-Q" "--eval" (prin1-to-string form)))
         (proc (let ((process-connection-type nil))
                 (apply #'start-process "emacs" buf "emacs" program-args))))))

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
  (define-key global-map [remap find-file] 'helm-find-files)
  (define-key global-map [remap occur] 'helm-occur)
  (define-key global-map [remap list-buffers] 'helm-buffers-list)
  (define-key lisp-interaction-mode-map
    [remap indent-for-tab-command] 'helm-lisp-completion-at-point-or-indent)
  (define-key emacs-lisp-mode-map
    [remap indent-for-tab-command] 'helm-lisp-completion-at-point-or-indent))





(vanilla-emacs 'vanilla-helm)
