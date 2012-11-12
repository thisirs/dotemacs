;; shortcut for reverting a buffer
(global-set-key (kbd "C-x C-r") 'revert-buffer)

(global-set-key (kbd "<C-kp-6>") 'enlarge-window-horizontally)
(global-set-key (kbd "<C-kp-4>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-kp-2>") 'enlarge-window)
(global-set-key (kbd "<C-kp-8>") 'shrink-window)

(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

(global-set-key (kbd "C-z") 'shell)

;; move between windows with meta-arrows
;; (windmove-default-keybindings 'meta)
(global-set-key (kbd "s-b") 'windmove-left)
(global-set-key (kbd "s-f") 'windmove-right)
(global-set-key (kbd "s-p") 'windmove-up)
(global-set-key (kbd "s-n") 'windmove-down)

;; quick jump to useful buffers
(defmacro shortcut (keybinding target &optional launch return)
  "Make KEYBINDING show TARGET. TARGET can be either a file path,
a buffer name or a form. Optional argument LAUNCH is used in case
the target does not exist. RETURN is a predicate telling if the
target is showed already. In that case we load window
configuration saved the first time the target was showed."
  (declare (debug defun))
  (when (stringp target)
    (if (file-name-absolute-p target)
        (setq return `(equal
                       (file-truename buffer-file-name)
                       (file-truename ,target))
              target `(and (file-exists-p ,target)
                           (find-file (file-truename ,target))))
      (setq return `(eq (current-buffer) (get-buffer ,target))
            target `(and (get-buffer ,target)
                         (switch-to-buffer (get-buffer ,target))))))
  (cond
   ((and (not ',launch) (not ',return))
    `(global-set-key
      (kbd ,keybinding)
      (lambda ()
        ,target)))
   ((not ',return)
    `(global-set-key
      (kbd ,keybinding)
      (lambda ()
        (unless ,target
          ,launch))))
   ((not ',launch)
    (global-set-key
     (kbd ,keybinding)
     (lambda ()
       (interactive)
       (if (ignore-errors ,return)
           (jump-to-register ?x)
         (window-configuration-to-register ?x)
         (unless ,target
           (error "Non-existent target"))))))
   (t
    `(global-set-key
      (kbd ,keybinding)
      (lambda ()
        (interactive)
        (if (ignore-errors ,return)
            (jump-to-register ?x)
          (window-configuration-to-register ?x)
          (unless ,target
            (if ',launch
                (if (yes-or-no-p "Non-existent target. Execute form? ")
                    ,launch)
              (error "Non-existent target")))))))))

(shortcut "s-s s" "*scratch*")
(shortcut "s-s e" "~/.emacs.d/init.el")
(shortcut "s-s m" "*Messages*")
(shortcut "s-s t" ":home" (twit))
(shortcut "s-s g" "*Group*" (gnus))
(shortcut "s-s i"
          (erc-track-switch-buffer 1)
          (erc-start-or-switch 1)
          (memq (current-buffer)
                erc-buffer-list))

(global-set-key (kbd "C-x à") 'delete-other-windows)
(global-set-key (kbd "C-x C-à") 'delete-other-windows)
(global-set-key (kbd "C-,") 'other-window)

(global-set-key (kbd "C-c C-t") 'ring-transparency)

;; automatically indent wherever I am
(global-set-key (kbd "RET") 'newline-and-indent)

(global-set-key [\C-home] 'beginning-of-buffer)
(global-set-key [\C-end] 'end-of-buffer)

;; fuck occur and word isearch
(global-set-key (kbd "M-s") 'backward-kill-word)

(global-set-key [(control tab)] 'other-window)

;; split screen and switch to it!
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

;; replace-string and replace-regexp need a key binding
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

(create-flash-binding "<f9>")
(create-flash-binding "<f10>")
(create-flash-binding "<f11>")
(create-flash-binding "<f12>")

;; From https://github.com/magnars/.emacs.d.git
(defmacro create-simple-keybinding-command (name key)
  `(progn (defmacro ,name (&rest fns)
            (list 'global-set-key (kbd ,key)
                  `(lambda ()
                     (interactive)
                     ,@fns)))

          (defmacro ,(intern (concat (symbol-name name) "e")) (&rest fns)
            (list 'global-set-key (kbd ,key)
                  `(lambda ()
                     (interactive)
                     (message "%s"
                              (progn
                                ,@fns)))))))

(create-simple-keybinding-command f9 "<f9>")
(create-simple-keybinding-command f10 "<f10>")
(create-simple-keybinding-command f11 "<f11>")
(create-simple-keybinding-command f12 "<f12>")

(provide 'init-bindings)
