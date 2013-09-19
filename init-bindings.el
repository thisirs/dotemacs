;; Shortcut for reverting a buffer
(global-set-key (kbd "C-x C-r") 'revert-buffer)

(global-set-key (kbd "<C-kp-4>") 'enlarge-window-horizontally)
(global-set-key (kbd "<C-kp-6>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-kp-8>") 'enlarge-window)
(global-set-key (kbd "<C-kp-2>") 'shrink-window)
(global-set-key (kbd "<C-kp-0>") 'transpose-buffers)

(global-set-key (kbd "<C-kp-multiply>") 'transpose-frame)

(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

(with-emacs-version< "24.3"
  (global-set-key (kbd "M-g c") 'goto-char))

(global-set-key (kbd "M-g f") 'find-grep)

(global-set-key (kbd "C-z") 'shell)

;; Move between windows with meta-arrows
;; (windmove-default-keybindings 'meta)
(global-set-key (kbd "s-b") 'windmove-left)
(global-set-key (kbd "s-f") 'windmove-right)
(global-set-key (kbd "s-p") 'windmove-up)
(global-set-key (kbd "s-n") 'windmove-down)

;; Move more quickly
(global-set-key (kbd "C-S-n") (lambda () (interactive) (next-line 5)))
(global-set-key (kbd "C-S-p") (lambda () (interactive) (previous-line 5)))
(global-set-key (kbd "C-S-f") (lambda () (interactive) (forward-char 5)))
(global-set-key (kbd "C-S-b") (lambda () (interactive) (backward-char 5)))

(global-set-key (kbd "C-x à") 'delete-other-windows)
(global-set-key (kbd "C-x C-à") 'delete-other-windows)
(global-set-key (kbd "C-,") 'other-window)

(global-set-key (kbd "C-c C-t") 'ring-transparency)

;; If electric-indent-mode is not available
(with-emacs-version< "24.1"
  (global-set-key (kbd "RET") 'newline-and-indent))

(global-set-key [\C-home] 'beginning-of-buffer)
(global-set-key [\C-end] 'end-of-buffer)

;; Fuck occur and word isearch
(global-set-key (kbd "M-s") 'backward-kill-word)

(global-set-key [(control tab)] 'other-window)

;; Split window and switch to newly created one
(global-set-key (kbd "C-x 3")
                (lambda nil
                  (interactive)
                  (split-window-horizontally)
                  (other-window 1)))

(global-set-key (kbd "C-x 2")
                (lambda nil
                  (interactive)
                  (split-window-vertically)
                  (other-window 1)))

(global-set-key (kbd "C-x C-v") 'find-file-other-window)

;; Binding for `replace-string' and `replace-regexp'
(global-set-key (kbd "C-c s") 'replace-string)
(global-set-key (kbd "C-c r") 'replace-regexp)

(defmacro create-flash-binding (key)
  "Make key `key' boundable to a complex command. Select the
complex command by typing C-`key'. Useful for example to repeat
an eval from M-:. Reuses the code from `repeat-complex-command'."
  `(global-set-key
    (kbd ,(concat "C-" key))
    (lambda ()
      (interactive)
      (lexical-let ((elt (nth 0 command-history))
                    newcmd)
        (if elt
            (progn
              (setq newcmd
                    (let ((print-level nil)
                          (minibuffer-history-position 0)
                          (minibuffer-history-sexp-flag (1+ (minibuffer-depth))))
                      (unwind-protect
                          (read-from-minibuffer
                           "Redo: " (prin1-to-string elt) read-expression-map t
                           (cons 'command-history 0))

                        ;; If command was added to command-history as a
                        ;; string, get rid of that.  We want only
                        ;; evaluable expressions there.
                        (if (stringp (car command-history))
                            (setq command-history (cdr command-history))))))

              ;; If command to be redone does not match front of history,
              ;; add it to the history.
              (or (equal newcmd (car command-history))
                  (setq command-history (cons newcmd command-history)))
              (global-set-key
               (kbd ,key)
               (lambda ()
                 (interactive)
                 (message "%s" (eval newcmd)))))
          (if command-history
              (error "Argument %d is beyond length of command history" 0)
            (error "There are no previous complex commands to repeat")))))))

(create-flash-binding "<f11>")
(create-flash-binding "<f12>")

(defmacro create-simple-keybinding-command (name &optional key)
  "Define two macros `<NAME>' and `<NAME>e' that bind KEY to the
body passed in argument."
  (unless key (setq key `[,name]))
  `(progn
     (defmacro ,name (&rest fns)
       ,(format "Execute FNS when %s is pressed. If FNS is a command symbol, call it interactively." name)
       (let ((command (if (and (eq (length fns) 1)
                               (commandp (car fns) t))
                          `',(car fns)
                        `(lambda ()
                           (interactive)
                           ,@fns))))
         `(global-set-key ,,key ,command)))
     (defmacro ,(intern (concat (symbol-name name) "e")) (&rest fns)
       ,(format "Execute FNS when %s is pressed. If FNS is a command symbol, call it interactively. Show the result in minibuffer." name)
       (let ((command (if (and (eq (length fns) 1)
                               (commandp (car fns) t))
                          `',(car fns)
                        `(lambda ()
                           (interactive)
                           (message "%s"
                                    (progn
                                      ,@fns))))))
         `(global-set-key ,,key ,command)))))

(create-simple-keybinding-command f9)
(create-simple-keybinding-command f10)

;; Align your code in a pretty way.
(global-set-key (kbd "C-x /") 'align-regexp)
(global-set-key (kbd "C-c e b") 'eval-buffer)
(global-set-key (kbd "C-c e d") 'toggle-debug-on-error)
(global-set-key (kbd "C-c e l") 'find-library)
(global-set-key (kbd "C-c e r") 'eval-region)
(global-set-key (kbd "C-c e q") 'toggle-debug-on-quit)
(global-set-key (kbd "C-c e g") 'toggle-debug-on-quit)
(global-set-key (kbd "C-c e m") 'macrostep-expand)
(global-set-key (kbd "C-c e L") 'elint-current-buffer)
(global-set-key (kbd "C-c e t") 'ert-run-tests-interactively)

(global-set-key (kbd "C-c d k") 'describe-key)
(global-set-key (kbd "C-c d v") 'describe-variable)
(global-set-key (kbd "C-c d f") 'describe-function)
(global-set-key (kbd "C-c d a") 'helm-apropos)

(global-set-key (kbd "C-h") (kbd "DEL"))

(provide 'init-bindings)
