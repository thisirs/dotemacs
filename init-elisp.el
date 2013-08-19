;; Eldoc mode
(autoload 'turn-on-eldoc-mode "eldoc" nil t)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)

(setq eval-expression-print-length nil)
(setq eval-expression-print-level nil)

(defun eval-region-or-buffer ()
  (interactive)
  (cond
   (mark-active
    (call-interactively 'eval-region)
    (message "Region evaluated!")
    (setq deactivate-mark t))
   (t
    (eval-buffer)
    (message "Buffer evaluated!"))))

(define-key emacs-lisp-mode-map (kbd "C-x E") 'eval-region-or-buffer)

;; Custom name for bookmark when in a defun
(defun emacs-lisp-custom-record-function ()
  (set (make-local-variable 'bookmark-make-record-function)
       (lambda (&optional no-file no-context posn)
         (let (defun record)
           (setq record (bookmark-make-record-default no-file no-context posn))
           (ignore-errors
             (save-excursion
               (end-of-defun)
               (beginning-of-defun)
               (setq defun (read (current-buffer)))))
           (if (eq (car defun) 'defun)
               (setcar record (format "%s" (cadr defun))))
           record))))

(defun emacs-lisp-add-keywords ()
  (font-lock-add-keywords
   nil
   '(("\\<\\(FIXME\\|TODO\\|BUG\\)"
      1 font-lock-warning-face prepend)))
  ;; (font-lock-add-keywords
  ;;  nil
  ;;  '(("\\<\\(add-hook\\|setq\\)\\>"
  ;;     1 font-lock-keyword-face prepend)))
  )

(defun emacs-lisp-custom-hippie-expand ()
  (set (make-local-variable 'hippie-expand-try-functions-list)
       '(yas/hippie-try-expand
         try-complete-file-name-partially
         try-complete-file-name
         try-expand-dabbrev-closest-first
         try-expand-dabbrev-visible
         try-complete-lisp-symbol-partially
         try-complete-lisp-symbol
         try-expand-dabbrev
         try-expand-dabbrev-all-buffers
         try-expand-dabbrev-from-kill
         try-complete-ispell)))

(add-hook 'emacs-lisp-mode-hook 'emacs-lisp-custom-record-function)
(add-hook 'emacs-lisp-mode-hook 'emacs-lisp-add-keywords)
(add-hook 'emacs-lisp-mode-hook 'emacs-lisp-custom-hippie-expand)

(defun auto-byte-recompile ()
  "If the current buffer is in emacs-lisp-mode and there already exists an `.elc'
file corresponding to the current buffer file, then recompile the file."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))

(add-hook 'after-save-hook 'auto-byte-recompile)

(provide 'init-elisp)

