(defun shell-bind (key command &optional msg interactive)
  "Bind a shell command to a global key sequence.

KEY is the key sequence to bind, as a string or vector.
COMMAND is the shell command to execute when the key is pressed.
Optional MSG, if non-nil and non-empty, is displayed after execution.
Optional INTERACTIVE, if non-nil, indicates the function is called interactively.

When called interactively, prompts for KEY, COMMAND, and MSG.
Displays a confirmation message after binding the key.

Example:
  (shell-bind \"C-c s\" \"ls -l\" \"List files\")
  ; Binds C-c s to run `ls -l` and show \"List files\" as a message."
  (interactive "KSet key globally: \nsSet key %s globally to shell command: \nsMessage: \np")
  (when interactive
    (setq key (key-description key)))
  (keymap-global-set key
                     `(lambda ()
                        (interactive)
                        (shell-command ,command)
                        (or (null ,msg) (string-empty-p ,msg) (message "%s" ,msg))))
  (when interactive
    (message "Bound %s to `%s`" key command)))

;; Shortcut for reverting a buffer
(keymap-global-set "C-x C-r" #'revert-buffer-quick)
(keymap-global-set "C-x k" #'kill-current-buffer)

(keymap-global-set "C-<left>" #'enlarge-window-horizontally)
(keymap-global-set "C-<right>" #'shrink-window-horizontally)
(keymap-global-set "C-<up>" #'enlarge-window)
(keymap-global-set "C-<down>" #'shrink-window)

(keymap-global-set "C-/" #'transpose-buffers)

(keymap-global-set "M-p" #'scroll-down-command)
(keymap-global-set "M-n" #'scroll-up-command)

;; A complementary binding to the apropos-command (C-h a)
(define-key 'help-command "A" #'apropos)

;; Toggle menu-bar visibility
(keymap-global-set "<f8>" #'menu-bar-mode)


(keymap-global-set "C-," #'other-window)
(keymap-global-set "M-o" #'other-window)

(keymap-global-set "M-g r" #'recompile)
(keymap-global-set "M-g c" #'compile)

(keymap-global-set "C-c h" help-map)

(keymap-global-set "<remap> <move-beginning-of-line>" #'beginning-of-line-or-indentation)

;; Move more quickly
(keymap-global-set "C-S-n" (lambda () (interactive) (forward-line 5)))
(keymap-global-set "C-S-p" (lambda () (interactive) (forward-line -5)))
(keymap-global-set "C-S-f" (lambda () (interactive) (forward-char 5)))
(keymap-global-set "C-S-b" (lambda () (interactive) (backward-char 5)))

(keymap-global-set "C-c C-t" #'ring-transparency)

;; If electric-indent-mode is not available
(with-emacs-version< "24.1"
  (keymap-global-set "RET" #'newline-and-indent))

(keymap-global-set "C-x TAB" #'indent-region-or-buffer)

;; Fuck occur and word isearch
(keymap-global-set "M-s" #'backward-kill-word)

;; Make upcase and co work with region
(keymap-global-set "<remap> <upcase-word>" #'upcase-dwim)
(keymap-global-set "<remap> <downcase-word>" #'downcase-dwim)
(keymap-global-set "<remap> <capitalize-word>" #'capitalize-dwim)

(keymap-global-set "C-<tab>" #'other-window)

;; Binding for `replace-string'
(keymap-global-set "C-c s" #'replace-string)

(defmacro create-flash-binding (key)
  "Make KEY boundable to a command.

Select the command by pressing Control + KEY. Invoke the command
by pressing KEY."
  `(keymap-global-set
    ,(concat "C-" key)
    (lambda ()
      (interactive)
      (lexical-let ((cmd (read--expression ,(format "Bind %s to: " key))))
        (keymap-global-set
         ,key
         (lambda ()
           (interactive)
           (message "%s" (eval cmd))))))))

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
         `(keymap-global-set ,,key ,command)))
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
         `(keymap-global-set ,,key ,command)))))

;; Creates f9 and f9e functions
(create-simple-keybinding-command f9)
(create-simple-keybinding-command f10)


;; For example: (f9 (with-region-or-line s-snake-case s-upcase))
;; When pressing f9, snake-case and upcase region or line
(defmacro with-region-or-line (&rest syms)
  "Apply functions SYMS on current region or line."
  `(let* ((region (if (region-active-p)
                      (delete-and-extract-region (region-beginning) (region-end))
                    (delete-and-extract-region (line-beginning-position) (line-end-position))))
          (new-text region))
     (condition-case e
         (progn
           (mapc (lambda (sym) (setq new-text (funcall sym new-text)))
                 (quote ,syms))
           (insert new-text))
       (error (progn
                (message "Error in with-region-or-line: %s" e)
                (insert region))))))

;; Align your code in a pretty way.
(keymap-global-set "C-x /" #'align-regexp)

(keymap-global-set "M-!" #'async-shell-command)
(keymap-global-set "C-x C-d" #'duplicate-line)
(keymap-global-set "C-c f" #'flush-lines)
(keymap-global-set "C-c k" #'keep-lines)


(keymap-global-set "C-c e k" #'find-function-on-key)
(keymap-global-set "C-c e b" #'eval-buffer)
(keymap-global-set "C-c e d" #'toggle-debug-on-error)
(keymap-global-set "C-c e r" #'eval-region)
(keymap-global-set "C-c e q" #'toggle-debug-on-quit)
(keymap-global-set "C-c e g" #'toggle-debug-on-quit)
(keymap-global-set "C-c e L" #'elint-current-buffer)
(keymap-global-set "C-c e t" #'ert-run-tests-interactively)
(keymap-global-set "C-c e l" #'find-library)

(provide 'init-bindings)
