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
  "Run a new emacs with loaded features FEATURES. Features'
settings are described by the macro `new-vanilla'."
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
  (define-key global-map [remap find-file] 'helm-find-files)
  (define-key global-map [remap occur] 'helm-occur)
  (define-key global-map [remap list-buffers] 'helm-buffers-list)
  (define-key lisp-interaction-mode-map
    [remap indent-for-tab-command] 'helm-lisp-completion-at-point-or-indent)
  (define-key emacs-lisp-mode-map
    [remap indent-for-tab-command] 'helm-lisp-completion-at-point-or-indent))

(new-vanilla expand-region
  (add-to-list 'load-path
               (expand-file-name "~/.emacs.d/site-lisp/expand-region.el"))
  (require 'expand-region)
  (global-set-key (kbd "C-=") 'er/expand-region))

(provide 'init-vanilla)



