(defmacro shell-bind (key cmd &optional msg)
  `(global-set-key (kbd ,key)
                   (lambda ()
                     (interactive)
                     (shell-command ,cmd)
                     ,@(if msg `((minibuffer-message ,msg))))))

(shell-bind "<f4>" "dbus-spotify playpause" "play/paused")
(shell-bind "<f5>" "dbus-spotify previous" "previous")
(shell-bind "<f6>" "dbus-spotify next" "next")

(defun mpv-bindings ()
  (interactive)
  (shell-bind "<f4>" "xdotool key --window \"$(xdotool search --class mpv | head -1)\" p" "play/paused")
  (shell-bind "<f5>" "xdotool key --window \"$(xdotool search --class mpv | head -1)\" Left" "advance")
  (shell-bind "<f6>" "xdotool key --window \"$(xdotool search --class mpv | head -1)\" Right" "next"))

;; Shortcut for reverting a buffer
(global-set-key (kbd "C-x C-r") #'revert-buffer-quick)
(global-set-key (kbd "C-x k") #'kill-this-buffer)

(global-set-key (kbd "<C-kp-4>") #'enlarge-window-horizontally)
(global-set-key (kbd "<C-kp-6>") #'shrink-window-horizontally)
(global-set-key (kbd "<C-kp-8>") #'enlarge-window)
(global-set-key (kbd "<C-kp-2>") #'shrink-window)

(global-set-key (kbd "<C-kp-divide>") #'transpose-buffers)

(global-set-key (kbd "M-p") #'scroll-down-command)
(global-set-key (kbd "M-n") #'scroll-up-command)

;; A complementary binding to the apropos-command (C-h a)
(define-key 'help-command "A" #'apropos)

;; Toggle menu-bar visibility
(global-set-key (kbd "<f8>") #'menu-bar-mode)

(global-set-key (kbd "C-x à") #'delete-other-windows)
(global-set-key (kbd "C-x C-à") #'delete-other-windows)
(global-set-key (kbd "C-,") #'other-window)
(global-set-key (kbd "M-o") #'other-window)

(global-set-key (kbd "M-g r") #'recompile)
(global-set-key (kbd "M-g c") #'compile)

(global-set-key (kbd "C-c h") help-map)

(global-set-key [remap move-beginning-of-line]
                #'beginning-of-line-or-indentation)

;; Move more quickly
(global-set-key (kbd "C-S-n") (lambda () (interactive) (forward-line 5)))
(global-set-key (kbd "C-S-p") (lambda () (interactive) (forward-line -5)))
(global-set-key (kbd "C-S-f") (lambda () (interactive) (forward-char 5)))
(global-set-key (kbd "C-S-b") (lambda () (interactive) (backward-char 5)))

(global-set-key (kbd "C-c C-t") #'ring-transparency)

;; If electric-indent-mode is not available
(with-emacs-version< "24.1"
  (global-set-key (kbd "RET") #'newline-and-indent))

(global-set-key (kbd "C-x TAB") #'indent-region-or-buffer)

(global-set-key [\C-home] #'beginning-of-buffer)
(global-set-key [\C-end] #'end-of-buffer)

;; Fuck occur and word isearch
(global-set-key (kbd "M-s") #'backward-kill-word)

;; Make upcase and co work with region
(global-set-key [remap upcase-word] #'upcase-dwim)
(global-set-key [remap downcase-word] #'downcase-dwim)
(global-set-key [remap capitalize-word] #'capitalize-dwim)

(global-set-key [(control tab)] #'other-window)

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

(global-set-key (kbd "C-x C-v") #'find-file-other-window)

;; Binding for `replace-string'
(global-set-key (kbd "C-c s") #'replace-string)

(defmacro create-flash-binding (key)
  "Make KEY boundable to a command.

Select the command by pressing Control + KEY. Invoke the command
by pressing KEY."
  `(global-set-key
    (kbd ,(concat "C-" key))
    (lambda ()
      (interactive)
      (lexical-let ((cmd (read--expression ,(format "Bind %s to: " key))))
        (global-set-key
         (kbd ,key)
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
(global-set-key (kbd "C-x /") #'align-regexp)

(global-set-key (kbd "M-!") #'async-shell-command)
(global-set-key (kbd "C-x C-d") #'duplicate-line)
(global-set-key (kbd "C-c f") #'flush-lines)
(global-set-key (kbd "C-c k") #'keep-lines)


(global-set-key (kbd "C-c e k") #'find-function-on-key)
(global-set-key (kbd "C-c e b") #'eval-buffer)
(global-set-key (kbd "C-c e d") #'toggle-debug-on-error)
(global-set-key (kbd "C-c e r") #'eval-region)
(global-set-key (kbd "C-c e q") #'toggle-debug-on-quit)
(global-set-key (kbd "C-c e g") #'toggle-debug-on-quit)
(global-set-key (kbd "C-c e L") #'elint-current-buffer)
(global-set-key (kbd "C-c e t") #'ert-run-tests-interactively)
(global-set-key (kbd "C-c e l") #'find-library)

(defun kmacro-reset-counter ()
  (interactive)
  (kmacro-set-counter 0))

(global-set-key (kbd "C-x C-k C-r") #'kmacro-reset-counter)

(provide 'init-bindings)
